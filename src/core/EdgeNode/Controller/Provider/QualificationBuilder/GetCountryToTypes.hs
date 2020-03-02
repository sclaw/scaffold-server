{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes
       (controller, EdgeNodeQualificationDegreeCapture) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.Provider as Provider

import KatipController
import Database.Transaction
import Control.Lens
import Pretty
import Katip
import TH.Proto
import Data.Coerce 
import Data.Swagger.Internal.Schema
import GHC.Generics hiding (from)
import Data.Aeson
import Control.Lens.Iso.Extended

newtype EdgeNodeQualificationDegreeCapture = EdgeNodeQualificationDegreeCapture EdgeNodeQualificationDegree
  deriving newtype Generic

instance ToSchema EdgeNodeQualificationDegreeCapture

instance ToJSON EdgeNodeQualificationDegreeCapture where
  toJSON x = String $ x^.coerced.isoEdgeNodeQualificationDegree.stext

controller 
  :: EdgeNodeAcademicArea 
  -> EdgeNodeCountry
  -> KatipController 
     (Response 
      [EdgeNodeQualificationDegreeCapture])
controller area country = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask 
  resp <- katipTransaction hasql $ 
    statement Provider.getCountryToTypes 
    (area, country)
  $(logTM) DebugS (logStr ("degrees: " ++ mkPretty mempty resp))
  return $ Ok $ fmap coerce resp