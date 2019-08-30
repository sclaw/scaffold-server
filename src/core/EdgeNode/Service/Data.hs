{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Service.Data (ServiceLanguage (..)) where

import EdgeNode.Service.Service

import TH.Mk
import GHC.Generics

mkSRGEqEnum ''Language "Service"
mkFromHttpApiDataEnum ''ServiceLanguage
mkParamSchemaEnum ''ServiceLanguage