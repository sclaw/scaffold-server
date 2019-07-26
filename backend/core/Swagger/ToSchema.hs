{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Swagger.ToSchema () where

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