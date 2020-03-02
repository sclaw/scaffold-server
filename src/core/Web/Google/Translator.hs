{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Translator (Items (..), GoogleLanguage (..)) where

import Web.Google.Translator.Country

import TH.Mk
import GHC.Generics hiding (from, to)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Vector as V
import Control.Lens
import Control.Lens.Iso.Extended

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
mkEnumConvertor ''GoogleLanguage
mkFromHttpApiDataEnum ''GoogleLanguage [|from stext.from isoGoogleLanguage.to Right|]
mkParamSchemaEnum ''GoogleLanguage