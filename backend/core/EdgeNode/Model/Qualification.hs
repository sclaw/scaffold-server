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
       ) where

import EdgeNode.Provider.Qualification

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
     { exGradeRangeGrade :: !QualificationGrade
     , exGradeRangeRank :: !Word32 
     } deriving stock Generic

deriveToSchemaAndJSON ''ExGradeRange
derivePrimitivePersistField ''ExGradeRange [| jsonb |]
deriveWrappedPrimitivePersistField ''QualificationId



