{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DeriveGeneric  #-}

module EdgeNode.Model.Qualification 
       ( QualificationProvider
       , QualificationDependency
       , ExGradeRange (..)
       , module EdgeNode.Provider.Qualification
       ) where

import EdgeNode.Provider.Qualification

import TH.Generator
import Data.Word
import GHC.Generics

data QualificationProvider  
data QualificationDependency

-- json: "{"grade":{"value":{"char":"2"}},"rank":2}"    
data ExGradeRange = 
     ExGradeRange
     { exGradeRangeGrade :: !Grade
     , exGradeRangeRank :: !Word32 
     } deriving stock Generic

deriveToSchemaAndJSON ''ExGradeRange