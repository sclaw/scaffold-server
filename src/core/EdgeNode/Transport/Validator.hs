{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Transport.Validator 
       (qualificationBuilder) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Qualification
import EdgeNode.Transport.Error

import Data.Aeson.WithField
import Data.Validation
import qualified Data.Text as T

data QualificationBuilderError = TitleEmpty  

instance AsError QualificationBuilderError where 
    asError TitleEmpty = asError @T.Text "qualification title shouldn't be empty"

qualificationBuilder 
  :: WithField "branch" 
     (Id "branch") 
     QualificationBuilder 
  -> Validation [QualificationBuilderError] ()
qualificationBuilder _ = undefined