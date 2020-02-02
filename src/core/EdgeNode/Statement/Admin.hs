{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Statement.Admin 
       ( ProviderRegistrationExt (..) 
       , newProvider)
        where

import qualified Hasql.Statement as HS
import Hasql.TH
import Control.Lens
import TH.Mk
import qualified Data.Text.Lazy as LT
import Control.Lens.Iso.Extended
import Database.Transaction
import Data.Generics.Product.Fields
import Data.List
import GHC.Generics (Generic)
import Data.Int
import Data.DeriveTH
import Test.QuickCheck.Extended

data ProviderRegistrationExt = 
     ProviderRegistrationExt
     { providerRegistrationExtAdminEmail     :: !LT.Text 
     , providerRegistrationExtProviderUID    :: !LT.Text
     , providerRegistrationExtPassword       :: !LT.Text
     , providerRegistrationExtType           :: !LT.Text
     , providerRegistrationExtRegisterStatus :: !LT.Text
     } deriving Generic

mkEncoder ''ProviderRegistrationExt
derive makeArbitrary ''ProviderRegistrationExt

instance ParamsShow ProviderRegistrationExt where
  render x = intercalate "," 
    [ x^.field @"providerRegistrationExtAdminEmail".from stextl
    , x^.field @"providerRegistrationExtProviderUID".from stextl
    , x^.field @"providerRegistrationExtPassword".from stextl
    , x^.field @"providerRegistrationExtType".from stextl
    , x^.field @"providerRegistrationExtRegisterStatus".from stextl
    ]

newProvider :: HS.Statement ProviderRegistrationExt (Maybe Int64)
newProvider = lmap ((\x -> x & each %~ (^.lazytext) & _3 %~ (^.textbs)) . mkEncoderProviderRegistrationExt) statement
  where  
    statement = 
      [maybeStatement|
        with 
          getProvider as (
           insert into edgenode.provider (uid) 
           values ($1 :: text)
           on conflict do nothing
           returning id, 1 as m),
          getUser as (
           insert into auth.user
           (identifier, password, user_type)
           values (md5($1 :: text || $2 :: text), $3 :: bytea, $4 :: text)
           on conflict do nothing
           returning id, 1 as m)
        insert into edgenode.provider_user 
        (email, status, provider_id, user_id)
        select $2 :: text, $5 :: text, t.pi, t.ui from  
        (select p.id as pi, u.id as ui 
         from getProvider as p inner join getUser as u on p.m = u.m) as t
        returning (select id from getUser) :: int8|]