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
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.User
import EdgeNode.Transport.Iso
import EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes
       (EdgeNodeQualificationDegreeCapture(..))
import EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries
       (EdgeNodeCountryCapture (..))
import qualified EdgeNode.Transport.Error as Error

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
deriving instance Enum Allegiance

mkEncoder ''Profile
mkEncoder ''FullDay
mkArbitrary ''AddQualification
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
      (x^?_5._Just.from allegiance.to AllegianceMaybe)
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
    & _5 %~ maybe mempty (^.field @"allegianceMaybeValue".allegiance.from stext)
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
      & _5 %~ (^?_Just.field @"allegianceMaybeValue".allegiance)
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

addTrajectory :: HS.Statement (UserId, OnlyField "id" (Id "qualification")) ()
addTrajectory =
  lmap (bimap coerce coerce)
  [resultlessStatement|
    insert into edgenode.user_trajectory
    (user_fk, provider_branch_qualification_fk)
    values ($1 :: int8, $2 :: int8)|]

instance ParamsShow AddQualification where
  render qualification = qualification^.field @"addQualificationValue".lazytext.from stext

addQualification
  :: HS.Statement
      (UserId, [WithId (Id "qualification") AddQualification])
      [Id "user_qualification"]
addQualification = lmap mkTpl $ statement $ premap coerce list
  where
    mkTpl x =
      consT (x^._1.coerced) $
      V.unzip $
      V.fromList $
      (x^._2 <&> \(WithField i v) ->
        ( i^.coerced
        , v^.field @"addQualificationValue".lazytext))
    statement =
      [foldStatement|
        insert into edgenode.user_qualification
        (user_fk, provider_branch_qualification_fk, value)
        select $1 :: int8, qid, v
        from unnest($2 :: int8[], $3 :: text[]) as x(qid, v)
        returning id :: int8|]

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

getQualificationsByBranch :: HS.Statement (UserId, Id "branch") [WithId (Id "qualification") (OnlyField "title" T.Text)]
getQualificationsByBranch = lmap (bimap coerce coerce) $ statement $ premap mkQual list
  where
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
        where pb.id = $2 :: int8 and (not (array[$1 :: int8] <@ coalesce(uq.users, array[] :: int8[])))|]

purgeQualifications :: HS.Statement (Id "provider", UserId, [Id "qualification"]) ()
purgeQualifications = pure ()

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