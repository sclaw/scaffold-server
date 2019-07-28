{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module EdgeNode.Service.Data (ServiceLanguage (..)) where

import EdgeNode.Service.Service

import TH.Generator
import GHC.Generics
import Data.Swagger

deriveToSchemaAndJSONProtoEnum ''Language "Service"
mkFromHttpApiDataEnum ''ServiceLanguage

instance ToParamSchema ServiceLanguage