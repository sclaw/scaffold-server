{-# LANGUAGE TemplateHaskell #-}
module Data.Swagger.Schema.Extended (schemaOptions, deriveToSchema) where

import           Data.Aeson.Extended (aesonOptions)
import           Data.Swagger.Schema
import           Language.Haskell.TH

schemaOptions :: String -> SchemaOptions
schemaOptions = fromAesonOptions . aesonOptions

deriveToSchema :: Name -> Q [Dec]
deriveToSchema name =
  [d|
    instance ToSchema $ty where
      declareNamedSchema = genericDeclareNamedSchema (schemaOptions $sname)
  |]
  where
    ty = return (ConT name)
    strName = nameBase name
    sname = return (LitE (StringL strName))

