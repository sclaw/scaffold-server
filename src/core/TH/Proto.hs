{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module TH.Proto where

import EdgeNode.Lang
import EdgeNode.Country

import TH.Mk
import GHC.Generics
import Control.Lens

mkEnumConvertor ''Language
mkEnumConvertor ''Country

mkSRGEqEnum ''Country "Wrapper"
mkEnumConvertor ''WrapperCountry
mkParamSchemaEnum ''WrapperCountry
mkFromHttpApiDataEnum ''WrapperCountry