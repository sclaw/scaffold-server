{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Service.Data (ServiceLanguage (..)) where

import EdgeNode.Service.Service

import TH.Generator
import GHC.Generics

deriveSRGEnum ''Language "Service"
mkFromHttpApiDataEnum ''ServiceLanguage
mkParamSchemaEnum ''ServiceLanguage