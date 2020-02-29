{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Provider.QualificationBuilder.Create (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Transport.Qualification
import qualified EdgeNode.Transport.Validator as Validator  

import KatipController
import Katip
import Pretty
import Data.Traversable
import Data.Aeson.WithField
import Data.Bifunctor

controller 
  :: WithField "branch" 
     (Id "branch") 
     QualificationBuilder 
  -> KatipController (Response (Id "qualification"))
controller builder = do
  $(logTM) DebugS (logStr ("builder: " ++ mkPretty mempty builder))
  fromValidation . (first (map asError)) <$> for (Validator.qualificationBuilder builder) undefined
