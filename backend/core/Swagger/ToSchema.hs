{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Swagger.ToSchema () where

import qualified EdgeNode.Api.Http.Auth.Register as Reg
import qualified EdgeNode.Api.Http.Auth.SignIn as SignIn

import Data.Sequence
import Data.Proxy
import Control.Lens
import Data.Swagger
import Data.Typeable
import Control.Lens.Iso.Extended
import Data.Aeson.Types
import Data.Aeson.Unit

instance (ToSchema a, Typeable a) => ToSchema (Seq a) where
  declareNamedSchema _ = do 
    schema <- declareSchema (Proxy :: Proxy [a])
    let unique = typeRep (Proxy :: Proxy a)^.to show.stext
    let name = "Seq." <> unique
    return $ NamedSchema (Just name) schema

instance ToSchema Reg.Error
instance ToSchema SignIn.Error

instance ToSchema Value where 
  declareNamedSchema _ = do
    obj <- declareSchemaRef (Proxy :: Proxy Object)
    arr <- declareSchemaRef (Proxy :: Proxy Array)
    str <- declareSchemaRef (Proxy :: Proxy String)
    num <- declareSchemaRef (Proxy :: Proxy Int)
    bool <- declareSchemaRef (Proxy :: Proxy Bool)
    null <- declareSchemaRef (Proxy :: Proxy Unit)
    let schema = 
         NamedSchema (Just "Value") $ mempty
         & type_ .~ SwaggerObject
         & properties .~ 
           [("object", obj), ("arr", arr), ("str", str),
            ("num", num), ("bool", bool), ("null", null)
           ]
    return schema