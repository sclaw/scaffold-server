{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Statement.User
       ( getProfile
       , patchProfile
       , addTrajectory
       , addQualification
       , getDegreeTypesByCategory
       , getCountriesByDegreeType
       , getBranchesByCountry
       , getQualificationsByBranch
       , getQualificationList
       , purgeQualifications
       , getTrajectories
       , removeTrajectory
       , getUserQualificationValues
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.User
import EdgeNode.Transport.Iso
import EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes
       (EdgeNodeQualificationDegreeCapture(..))
import EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries
       (EdgeNodeCountryCapture (..))
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Transport.Qualification

import qualified Hasql.Statement as HS
import Auth
import Data.Aeson.WithField.Extended
import Hasql.TH
import Data.Coerce
import Control.Lens
import Data.Int
import Control.Lens.Iso.Extended
import qualified Protobuf.Scalar as Protobuf
import Database.Transaction
import TH.Mk
import Data.Time.Transport
import Data.Tuple.Extended
import Data.List
import Data.Word
import Data.Generics.Product.Fields
import Data.Aeson
import qualified Data.Vector.Extended as V
import Control.Foldl
import TH.Proto
import Data.String.Conv
import qualified Data.Text as T
import Data.Default.Class.Extended
import Data.Bifunctor

deriving instance Enum Gender

mkEncoder ''Profile
mkEncoder ''FullDay
mkArbitrary ''AddQualificationRequest
mkArbitrary ''EdgeNodeProviderCategory

mkFullDay x =
  case fromJSON x of
    Error e -> error $ "unable to convert json to FullDay, " ++ e
    Success x -> x

getProfile :: HS.Statement UserId (OptField "image" (Id "image") Profile)
getProfile = dimap (coerce @_ @Int64) mkProfile $ statement
  where
    mkProfile x =
      OptField $
      WithField (x^?_1._Just.coerced) $
      Profile
      (x^?_2._Just.from lazytext.to Protobuf.String)
      (x^?_3._Just.from lazytext.to Protobuf.String)
      (x^?_4._Just.from lazytext.to Protobuf.String)
      (mkFullDay (x^._7))
      (x^?_5._Just.from country.to AllegianceMaybe)
      (Just (GenderMaybe (x^._6.from gender)))
    statement =
      [singletonStatement|
        select
          eu.avatar_id :: int8?,
          eu.name :: text?,
          eu.middlename :: text?,
          eu.surname :: text?,
          eu.allegiance :: text?,
          eu.gender :: text,
          jsonb_build_object(
            'year', pfd.year,
            'month', pfd.month,
            'day', pfd.day) :: jsonb
        from auth.user as au
        inner join edgenode.user as eu
        on au.id = eu.user_id
        inner join public.full_day as pfd
        on eu.birthday_id = pfd.id
        where au.id = $1 :: int8|]

instance ParamsShow Profile where
  render profile = intercalate ", " $
    (mkEncoderProfile profile
    & _1 %~ (^._Just.field @"stringValue".lazytext.from stext)
    & _2 %~ (^._Just.field @"stringValue".lazytext.from stext)
    & _3 %~ (^._Just.field @"stringValue".lazytext.from stext)
    & _4 %~ maybe mempty (\x -> "[" ++ intercalate ", " (mkEncoderFullDay x^..each.integral.to show) ++ "]")
    & _5 %~ maybe mempty (^.field @"allegianceMaybeValue".country.from stext)
    & _6 %~ maybe mempty (^.field @"genderMaybeValue".gender.from stext))^..each

patchProfile :: HS.Statement (UserId, OptField "image" (Id "image") Profile) ()
patchProfile = lmap mkTpl statement
  where
    mkTpl (user_id, OptField (WithField img profile)) =
      consT (coerce user_id) $
      consT (fmap coerce img) $
      (mkEncoderProfile profile
      & _1 %~ (^?_Just.field @"stringValue".lazytext)
      & _2 %~ (^?_Just.field @"stringValue".lazytext)
      & _3 %~ (^?_Just.field @"stringValue".lazytext)
      & _4 %~ fmap toJSON
      & _5 %~ (^?_Just.field @"allegianceMaybeValue".country)
      & _6 %~ (^?_Just.field @"genderMaybeValue".gender))
    statement =
      [resultlessStatement|
        with get_full_day_id as
         (update edgenode.user set
           avatar_id = coalesce($2 :: int8?, avatar_id),
           name = coalesce($3 :: text?, name),
           middlename = coalesce($4 :: text?, middlename),
           surname = coalesce($5 :: text?, surname),
           allegiance = coalesce($7 :: text?, allegiance),
           gender = coalesce($8 :: text?, gender),
           modified = now()
          where "user_id" = $1 :: int8)
        update public.full_day set
         year = coalesce(cast(($6 :: jsonb?)->'year' as integer), year),
         month = coalesce(cast(($6 :: jsonb?)->'month' as integer), month),
         day = coalesce(cast(($6 :: jsonb?)->'day' as integer), day)
        where id = (select birthday_id from edgenode.user where "user_id" = $1 :: int8)|]

addTrajectory :: HS.Statement (UserId, OnlyField "id" (Id "qualification"), Double) Int64
addTrajectory =
  lmap (\x -> x & _1 %~ coerce & _2 %~ coerce) $
  [rowsAffectedStatement|
    with already as (
      select
        $1 :: int8 as user,
        $2 :: int8 as qual,
        exists(select
          from edgenode.user_qualification
          where "user_fk" = $1 :: int8 and
          provider_branch_qualification_fk = $2 :: int8) as flag)
    insert into edgenode.user_trajectory
    (user_fk, provider_branch_qualification_fk, compatibility)
    select n.user, n.qual, round(($3 :: float8) :: "numeric", 1) :: float8
    from (select $1 :: int8 as user, $2 :: int8 as qual, true as flag) as n
    inner join already as a
    on n.user = a.user and n.qual = a.qual
    where not (n.flag and a.flag)|]

instance ParamsShow AddQualificationRequest where
  render qualification = qualification^.field @"addQualificationRequestValue".lazytext.from stext

addQualification
  :: HS.Statement
      (UserId, [WithId (Id "qualification") AddQualificationRequest])
      [(Id "qualification", Id "user_qualification")]
addQualification = lmap mkTpl $ statement $ premap (bimap coerce coerce) list
  where
    mkTpl x =
      consT (x^._1.coerced) $
      V.unzip $
      V.fromList $
      (x^._2 <&> \(WithField i v) ->
        ( i^.coerced
        , v^.field @"addQualificationRequestValue".lazytext))
    statement =
      [foldStatement|
        insert into edgenode.user_qualification
        (user_fk, provider_branch_qualification_fk, value)
        select x.uid, x.qid, x.v
        from unnest(
          array_fill($1 :: int8, array[array_length($2 :: int8[], 1)]),
          $2 :: int8[],
          $3 :: text[]) as x(uid, qid, v)
        except
        select
          "user_fk",
          provider_branch_qualification_fk,
          unnest($3 :: text[])
        from edgenode.user_trajectory
        where "user_fk" = $1 :: int8
        returning provider_branch_qualification_fk :: int8, id :: int8|]

instance ParamsShow EdgeNodeProviderCategory where render = (^.isoEdgeNodeProviderCategory.to toS)

getDegreeTypesByCategory :: HS.Statement EdgeNodeProviderCategory [EdgeNodeQualificationDegreeCapture]
getDegreeTypesByCategory =
  lmap (^.isoEdgeNodeProviderCategory.to (toS @_ @T.Text)) $
  statement $
  premap (^.to (toS @T.Text @_).from isoEdgeNodeQualificationDegree.coerced) list
  where
    statement =
      [foldStatement|
        select distinct pbq.type :: text
        from edgenode.provider as p
        left join edgenode.provider_branch as pb
        on p.id = pb.provider_fk
        inner join edgenode.provider_branch_qualification as pbq
        on pb.id = pbq.provider_branch_fk
        where p.category = $1 :: text|]

getCountriesByDegreeType :: HS.Statement (EdgeNodeProviderCategory, EdgeNodeQualificationDegree) [EdgeNodeCountryCapture]
getCountriesByDegreeType = lmap convert $ statement $ premap (^.to (toS @T.Text @_).from isoEdgeNodeCountry.coerced) list
  where
    convert x =
      x & _1 %~ (^.isoEdgeNodeProviderCategory.to (toS @_ @T.Text))
        & _2 %~ (^.isoEdgeNodeQualificationDegree.to (toS @_ @T.Text))
    statement =
      [foldStatement|
        select distinct pb.country :: text
        from edgenode.provider as p
        left join edgenode.provider_branch as pb
        on p.id = pb.provider_fk
        inner join edgenode.provider_branch_qualification as pbq
        on pb.id = pbq.provider_branch_fk
        where p.category = $1 :: text and pbq.type = $2 :: text|]

getBranchesByCountry
  :: HS.Statement
     ( EdgeNodeProviderCategory
     , EdgeNodeQualificationDegree
     , EdgeNodeCountry)
     [WithId (Id "branch") (OnlyField "title" T.Text)]
getBranchesByCountry = lmap convert $ statement $ premap mkBranch list
  where
    convert x =
      x & _1 %~ (^.isoEdgeNodeProviderCategory.to (toS @_ @T.Text))
        & _2 %~ (^.isoEdgeNodeQualificationDegree.to (toS @_ @T.Text))
        & _3 %~ (^.isoEdgeNodeCountry.to (toS @_ @T.Text))
    mkBranch (ident, title) = WithField (coerce ident) (OnlyField title)
    statement =
      [foldStatement|
        select distinct pb.id :: int8, pb.title :: text
        from edgenode.provider as p
        left join edgenode.provider_branch as pb
        on p.id = pb.provider_fk
        inner join edgenode.provider_branch_qualification as pbq
        on pb.id = pbq.provider_branch_fk
        where p.category = $1 :: text and pbq.type = $2 :: text and pb.country = $3 :: text|]

getQualificationsByBranch :: HS.Statement (UserId, Id "branch", EdgeNodeQualificationDegree) [WithId (Id "qualification") (OnlyField "title" T.Text)]
getQualificationsByBranch = lmap transform $ statement $ premap mkQual list
  where
    transform x =
      x & _1 %~ coerce @_ @Int64
        & _2 %~ coerce @_ @Int64
        & _3 %~ (^.isoEdgeNodeQualificationDegree.to (toS @_ @T.Text))
    mkQual (ident, title) = WithField (coerce ident) (OnlyField title)
    statement =
      [foldStatement|
        select pbq.id :: int8, pbq.title :: text
        from edgenode.provider_branch as pb
        inner join edgenode.provider_branch_qualification as pbq
        on pb.id = pbq.provider_branch_fk
        left join (select provider_branch_qualification_fk as ident, array_agg("user_fk") as users
         from edgenode.user_qualification
         group by provider_branch_qualification_fk) as uq
        on pbq.id = uq.ident
        where pb.id = $2 :: int8 and (not (array[$1 :: int8] <@ coalesce(uq.users, array[] :: int8[]))) and pbq.type = $3 :: text|]

purgeQualifications :: HS.Statement (Id "provider", UserId, [Id "qualification"]) Int64
purgeQualifications = lmap transform statement
  where
    transform x =
      x & _1 %~ coerce @_ @Int64
        & _2 %~ coerce @_ @Int64
        & _3 %~ (V.fromList . Prelude.map (coerce @_ @Int64))
    statement =
      [rowsAffectedStatement|
        with qualification_ids as (
          select pbq.id as ident
          from edgenode.provider as p
          left join edgenode.provider_branch as pb
          on p.id = pb.provider_fk
          left join edgenode.provider_branch_qualification as pbq
          on pb.id = pbq.provider_branch_fk
          where p.id = $1 :: int8),
        ids as (
          select ids.ident
          from unnest($3 :: int8[]) as x(i)
          inner join qualification_ids as ids
          on x.i = ids.ident)
       delete from edgenode.user_qualification
       where "user_fk" = $2 :: int8 and provider_branch_qualification_fk = any(select ident from ids)|]

instance Default UserQualificationItem
instance Default UserQualificationItem_Element

data GetQualificationListError =
     GetQualificationListJsonDecodeError String
     deriving stock Show

instance Error.AsError GetQualificationListError where
  asError (GetQualificationListJsonDecodeError e) =
    Error.asError @T.Text $ "json decode error: " <> toS e

getQualificationList :: HS.Statement UserId (Either GetQualificationListError UserQualification)
getQualificationList = lmap coerce $ statement $ fmap (second UserQualification . sequence) (premap mkQual vector)
  where
    mkQual x =
      case flip fmap (sequence ((x^._4) <&> fromJSON)) $ \vs ->
        def @UserQualificationItem
        & field @"userQualificationItemProviderId" .~ x^._1.integral
        & field @"userQualificationItemProvider" .~ x^._2.from lazytext
        & field @"userQualificationItemDegreeType" .~ x^._3.to toS.from qualQualificationDegree
        & field @"userQualificationItemXs" .~ vs
      of
        Error e -> Left $ GetQualificationListJsonDecodeError e
        Success x -> Right x
    statement =
      [foldStatement|
        select
          p.id :: int8,
          p.title :: text,
          pbq.type :: text,
          array_agg(jsonb_build_object(
           'elementId', pbq.id,
           'title', pbq.title,
           'value', uq.value)) :: jsonb[]
        from edgenode.user_qualification as uq
        inner join edgenode.provider_branch_qualification as pbq
        on uq.provider_branch_qualification_fk = pbq.id
        inner join edgenode.provider_branch as pb
        on pbq.provider_branch_fk = pb.id
        inner join edgenode.provider as p
        on pb.provider_fk = p.id
        where uq.user_fk = $1 :: int8
        group by p.id, p.title, pbq.type
        order by p.title, pbq.type|]

getTrajectories :: HS.Statement UserId (Either T.Text UserTrajectories)
getTrajectories = lmap coerce $ statement $ fmap (fmap UserTrajectories . sequence) (premap mkUserTrajectory vector)
  where
    mkUserTrajectory x =
      flip fmap (sequence (V.map (mkEither . fromJSON @UserTrajectory_Dependency) (x^._8))) $
        (UserTrajectory
        (x^._1.(integral @Int64 @Word64).coerced)
        (x^._2.from lazytext)
        (x^._3.to toS.from qualQualificationDegree)
        (x^._4)
        (x^?_5._Just.from period.to UserTrajectoryPeriod)
        (x^?_6._Just.from currency.to UserTrajectoryCurrency)
        (x^?_7._Just.to (Protobuf.Double . fromIntegral)))
    mkEither (Success x) = Right x
    mkEither (Error e) = Left $ T.pack e
    statement =
      [foldStatement|
        with lacked_dependency as (
          select
            ut.provider_branch_qualification_fk as ident,
            array_agg(jsonb_build_object(
              'title', pbq.title,
              'depDegreeValue', pbqd.required_degree,
              'userDegreeValue', uq.value)) as deps
          from edgenode.provider_branch_qualification_dependency as pbqd
          left join edgenode.user_trajectory as ut
          on ut.provider_branch_qualification_fk = pbqd.dependency_fk
          inner join edgenode.provider_branch_qualification as pbq
          on pbq.id = pbqd.provider_branch_qualification_fk
          left join edgenode.user_qualification as uq
          on uq.provider_branch_qualification_fk =
             pbqd.provider_branch_qualification_fk
             and uq.user_fk = $1 :: int8
          where ut.user_fk = $1 :: int8
          group by ut.provider_branch_qualification_fk)
        select
          ut.id :: int8,
          pbq.title :: text,
          pbq.type :: text,
          ut.compatibility :: float8,
          pbqtf.period :: text?,
          pbqtf.currency :: text?,
          pbqtf.amount :: int8?,
          ld.deps :: jsonb[]
        from edgenode.user as u
        left join edgenode.user_trajectory as ut
        on u.user_id = ut.user_fk
        inner join edgenode.provider_branch_qualification as pbq
        on ut.provider_branch_qualification_fk = pbq.id
        left join edgenode.provider_branch_qualification_tuition_fees as pbqtf
        on pbq.id = pbqtf.provider_branch_qualification_fk
        and u.allegiance in (select * from jsonb_array_elements_text(pbqtf.countries))
        left join lacked_dependency as ld
        on ld.ident = ut.provider_branch_qualification_fk
        where ut.user_fk = $1 :: int8|]

removeTrajectory :: HS.Statement (Id "trajectory", UserId) ()
removeTrajectory =
  lmap (bimap coerce coerce) $
  [resultlessStatement|
    delete from edgenode.user_trajectory
    where id = $1 :: int8 and "user_fk" = $2 :: int8|]

getUserQualificationValues :: HS.Statement UserId (Maybe [(Int64, QualificationDegree, T.Text)])
getUserQualificationValues =
  lmap (coerce @_ @Int64) $
  statement $
  fmap wrapToMaybe (premap (& _2 %~ (toQualificationDegree . toS)) list)
  where
    statement =
      [foldStatement|
        select
          uq.provider_branch_qualification_fk :: int8,
          pbq.type :: text,
          uq.value :: text
        from edgenode.user_qualification as uq
        inner join edgenode.provider_branch_qualification as pbq
        on uq.provider_branch_qualification_fk = pbq.id
        where uq.user_fk = $1 :: int8|]
    wrapToMaybe [] = Nothing
    wrapToMaybe xs = Just xs