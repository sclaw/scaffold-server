{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module EdgeNode.Service.Data (ServiceLanguage (..)) where

import EdgeNode.Service.Service

import TH.Generator
import GHC.Generics

deriveToSchemaAndJSONProtoEnum ''Language "Service"
mkFromHttpApiDataEnum ''ServiceLanguage
mkParamSchemaEnum ''ServiceLanguage