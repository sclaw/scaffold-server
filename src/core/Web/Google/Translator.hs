{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Translator (Items (..), GoogleLanguage (..)) where

import Web.Google.Translator.Country

import TH.Mk
import GHC.Generics
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Vector as V

newtype Items = Items [T.Text]
  
instance FromJSON Items where
  parseJSON = withObject "Items.stage1" $ \o -> do 
    o' <- o .: "data"
    o'' <- o' .: "translations"
    let f v = 
         withObject 
         "Items.stage3" 
         (.: "translatedText") 
         (v V.! 0) 
    (Items . T.splitOn ",") `fmap` withArray "Items.stage2" f o''

mkSRGEqEnum ''Language "Google"
mkFromHttpApiDataEnum ''GoogleLanguage
mkParamSchemaEnum ''GoogleLanguage