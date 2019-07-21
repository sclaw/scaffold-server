{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Swagger.Proto () where

import qualified EdgeNode.Api.Http.Auth.Register as Reg
import qualified EdgeNode.Api.Http.Auth.SignIn as SignIn

import Data.Sequence
import Data.Proxy
import Control.Lens
import Data.Swagger
import Data.Typeable
import Control.Lens.Iso.Extended

instance (ToSchema a, Typeable a) => ToSchema (Seq a) where
  declareNamedSchema _ = do 
    schema <- declareSchema (Proxy :: Proxy [a])
    let unique = typeRep (Proxy :: Proxy a)^.to show.stext
    let name = "Seq." <> unique
    return $ NamedSchema (Just name) schema

instance ToSchema Reg.Error
instance ToSchema SignIn.Error
