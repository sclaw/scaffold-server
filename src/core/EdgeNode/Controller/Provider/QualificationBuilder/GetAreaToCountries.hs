{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries 
       (controller, EdgeNodeCountryCapture) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.Provider as Provider

import KatipController
import Database.Transaction
import Control.Lens
import Pretty
import Katip
import TH.Proto
import GHC.Generics hiding (from, to)
import Data.Aeson
import Control.Lens.Iso.Extended
import Data.Coerce 
import Data.Swagger.Internal.Schema
import Data.Proxy
import Data.Swagger hiding (Response)

newtype EdgeNodeCountryCapture = EdgeNodeCountryCapture EdgeNodeCountry
  deriving newtype Generic

instance ToSchema EdgeNodeCountryCapture where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    return $ NamedSchema (Just "EdgeNodeCountryCapture") $ mempty
      & type_ ?~ SwaggerString 
      & enum_ ?~ map (^.isoEdgeNodeCountry.stext.to String) [Russia .. UnitedKingdom]
        
instance ToJSON EdgeNodeCountryCapture where
  toJSON x = String $ x^.coerced.isoEdgeNodeCountry.stext

controller :: EdgeNodeAcademicArea -> KatipController (Response [EdgeNodeCountryCapture])
controller area = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask 
  resp <- katipTransaction hasql $ 
    statement Provider.getAreaToCountries area
  $(logTM) DebugS (logStr ("countries: " ++ mkPretty mempty resp))
  return $ Ok $ fmap coerce resp