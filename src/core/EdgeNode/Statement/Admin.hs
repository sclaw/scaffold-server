{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Statement.Admin
       ( ProviderRegistrationExt (..)
       , newProvider
       , resetPassword)
        where

import qualified Hasql.Statement as HS
import Hasql.TH
import Control.Lens
import TH.Mk
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Control.Lens.Iso.Extended
import Database.Transaction
import Data.Generics.Product.Fields
import Data.List
import GHC.Generics (Generic)
import Data.Int
import Test.QuickCheck.Extended
import Test.QuickCheck.Arbitrary.Generic

data ProviderRegistrationExt =
     ProviderRegistrationExt
     { providerRegistrationExtAdminEmail     :: !LT.Text
     , providerRegistrationExtProviderUID    :: !LT.Text
     , providerRegistrationExtTitle          :: !LT.Text
     , providerRegistrationExtCategory       :: !LT.Text
     , providerRegistrationExtPassword       :: !LT.Text
     , providerRegistrationExtType           :: !LT.Text
     , providerRegistrationExtRegisterStatus :: !LT.Text
     } deriving (Generic, Show)

mkEncoder ''ProviderRegistrationExt

instance Arbitrary ProviderRegistrationExt where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance ParamsShow ProviderRegistrationExt where
  render x = intercalate ","
    [ x^.field @"providerRegistrationExtAdminEmail".from stextl
    , x^.field @"providerRegistrationExtProviderUID".from stextl
    , x^.field @"providerRegistrationExtTitle".from stextl
    , x^.field @"providerRegistrationExtCategory".from stextl
    , x^.field @"providerRegistrationExtPassword".from stextl
    , x^.field @"providerRegistrationExtType".from stextl
    , x^.field @"providerRegistrationExtRegisterStatus".from stextl
    ]

newProvider :: HS.Statement ProviderRegistrationExt (Maybe Int64)
newProvider = lmap ((\x -> x & each %~ (^.lazytext) & _5 %~ (^.textbs)) . mkEncoderProviderRegistrationExt) statement
  where
    statement =
      [maybeStatement|
        with
          getProvider as (
           insert into edgenode.provider
           (uid, title, category)
           values ($2 :: text, $3 :: text, $4 :: text)
           on conflict do nothing
           returning id, 1 as m),
          getUser as (
           insert into auth.user
           (identifier, password, user_type, email)
           values (
             md5($1 :: text || $2 :: text),
             $5 :: bytea,
             $6 :: text,
             $1 :: text)
           on conflict do nothing
           returning id, 1 as m)
        insert into edgenode.provider_user
        (email, status, provider_id, user_id)
        select $1 :: text, $7 :: text, t.pi, t.ui from
        (select p.id as pi, u.id as ui
         from getProvider as p inner join getUser as u on p.m = u.m) as t
        returning (select id from getUser) :: int8|]

resetPassword :: HS.Statement (T.Text, T.Text, T.Text) Int64
resetPassword =
  lmap (& _3 %~ (^.textbs)) $
  [rowsAffectedStatement|
   update auth.user set
   password = $3 :: bytea,
   password_modified_tm = now()
   where id =
     (select pu.user_id from edgenode.provider_user as pu
      inner join edgenode.provider as p
      on pu.provider_id = p.id
      where pu.email = $2 :: text and p.uid = $1 :: text)|]