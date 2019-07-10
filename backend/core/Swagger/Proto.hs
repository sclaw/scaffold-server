{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Swagger.Proto () where

import Text.ProtocolBuffers.Basic
import Data.Proxy
import Control.Lens
import Data.Swagger
import Data.Typeable
import Control.Lens.Iso.Extended

instance ToSchema Utf8 where
  declareNamedSchema _ = do 
   schema <- declareSchema (Proxy :: Proxy String)
   return $ NamedSchema (Just "Utf8") schema

instance (ToSchema a, Typeable a) => ToSchema (Seq a) where
  declareNamedSchema _ = do 
    schema <- declareSchema (Proxy :: Proxy [a])
    let unique = typeRep (Proxy :: Proxy a)^.to show.stext
    let name = "Seq." <> unique
    return $ NamedSchema (Just name) schema