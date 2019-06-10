{-# LANGUAGE TemplateHaskell #-}

module Data.Swagger.Schema.Extended (deriveToSchema) where

import           Data.Swagger.Schema
import           Language.Haskell.TH

deriveToSchema :: Name -> Q [Dec]
deriveToSchema name =
  do 
    let ty = return (ConT name)
    [d|
       instance ToSchema $ty where
         declareNamedSchema = undefined
     |]