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

module EdgeNode.Statement.User
       ( getProfile
       , patchProfile
       , addTrajectory
       , addQualification
       , getDegreeTypesByCategory
       , getCountriesByDegreeType
       , getBranchesByCountry
       , getQualificationsByBranch
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.User
import EdgeNode.Transport.Iso
import EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes
       (EdgeNodeQualificationDegreeCapture(..))
import EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries
       (EdgeNodeCountryCapture (..))

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

getQualificationsByBranch :: HS.Statement (Id "branch") [WithId (Id "qualification") (OnlyField "title" T.Text)]
getQualificationsByBranch = lmap coerce $ statement $ premap mkQual list
  where
    mkQual (ident, title) = WithField (coerce ident) (OnlyField title)
    statement =
      [foldStatement|
        select pbq.id :: int8, pbq.title :: text
        from edgenode.provider_branch as pb
        inner join edgenode.provider_branch_qualification as pbq
        on pb.id = pbq.provider_branch_fk
        where pb.id = $1 :: int8|]