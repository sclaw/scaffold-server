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
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.User
import EdgeNode.Transport.Iso

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

deriving instance Enum Gender
deriving instance Enum Allegiance

mkEncoder ''Profile
mkEncoder ''FullDay

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