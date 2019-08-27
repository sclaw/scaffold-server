{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EdgeNode.Model.Qualification 
       ( QualificationProvider
       , QualificationDependency
       , ExGradeRange (..)
       , module EdgeNode.Provider.Qualification
       , mkRange
       ) where

import EdgeNode.Provider.Qualification
import EdgeNode.Category

import TH.Generator
import Data.Word
import GHC.Generics
import Database.Groundhog.Instances ()
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Control.Lens.Iso.Extended

data QualificationProvider  
data QualificationDependency

-- json: "{"grade":{"value":{"string":"2"}},"rank":2}"    
data ExGradeRange = 
     ExGradeRange
     { exGradeRangeGrade :: !(Either LSGrade QualificationGrade)
     , exGradeRangeRank :: !Word32 
     } deriving stock Generic

mkRange :: Either LSGrade QualificationGrade -> Range
mkRange (Left x) = Range $ Just $ RangeValueLanguage x
mkRange (Right x) = Range $ Just $ RangeValueQualification x

deriveToSchemaAndJSON ''ExGradeRange
derivePrimitivePersistField ''ExGradeRange [| jsonb |]
deriveWrappedPrimitivePersistField ''QualificationId