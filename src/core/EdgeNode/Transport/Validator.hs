{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Transport.Validator 
       ( qualificationBuilder
       , degreeToValues) where

import EdgeNode.Transport.Qualification
import EdgeNode.Transport.Error

import Data.Validation
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Maybe
import Control.Lens
import Data.Generics.Product.Fields
import qualified Data.Vector as V
import TextShow 

data QualificationBuilderError = 
       TitleEmpty
     | AcademicAreaEmpty
     | DegreeValueNotFoundAtQualificationDegree
     | QualificationNotFound 
     deriving stock Show

instance AsError QualificationBuilderError where 
  asError TitleEmpty = asError @T.Text "qualification title shouldn't be empty"
  asError AcademicAreaEmpty = asError @T.Text "at least one academic area should be set"
  asError DegreeValueNotFoundAtQualificationDegree = asError @T.Text "degree value mismatches given qualification degree"
  asError QualificationNotFound = asError @T.Text "qualification not found"

qualificationBuilder :: QualificationBuilder -> Validation [QualificationBuilderError] ()
qualificationBuilder builder 
  | isNothing (
      builder^.
      field @"qualificationBuilderQualification") 
    = Failure [QualificationNotFound]   
qualificationBuilder builder = checkTitle *> checkAcademicArea *> checkDegreeValue
  where 
    checkTitle 
      | LT.null (
        builder^?!
        field @"qualificationBuilderQualification".
        _Just.
        field @"qualificationTitle") 
        = Failure [TitleEmpty]
      | otherwise = pure ()
    checkAcademicArea 
      | V.null (
        builder^?!
        field @"qualificationBuilderQualification".
        _Just.
        field @"qualificationAreas") 
        = Failure [AcademicAreaEmpty]
      | otherwise = pure () 
    checkDegreeValue 
      | not $ checkDegreeValue' (builder^?!field @"qualificationBuilderQualification"._Just)
        = Failure [DegreeValueNotFoundAtQualificationDegree]
      | otherwise = pure ()  

checkDegreeValue' :: Qualification -> Bool
checkDegreeValue' Qualification  {..} = check qualificationDegreeValue 
  where
    degree = qualificationDegreeType^?!field @"enumerated"._Right
    check Nothing = True
    check (Just x) = elem (x^.field @"stringValue") (fromJust (lookup degree degreeToValues)) 

degreeToValues :: [(QualificationDegree, [LT.Text])]
degreeToValues = 
  [ (QualificationDegreeUnifiedStateExam, map (LT.fromStrict . showt) [1 .. 100 :: Int])
  , (QualificationDegreeAdvancedLevelGCE, ["A*", "A", "B", "C", "D", "E"])
  , (QualificationDegreeMagistr, ["Maj", "Std"])
  , (QualificationDegreeBakalavr, ["Maj", "Std"])
  , (QualificationDegreeBSc, ["1", "2:1", "2:2", "3"])
  , (QualificationDegreeBA, ["1", "2:1", "2:2", "3"])
  , (QualificationDegreeMSc, ["1", "2:1", "2:2", "3"])
  , (QualificationDegreeMA, ["1", "2:1", "2:2", "3"])
  , (QualificationDegreeMRes, ["1", "2:1", "2:2", "3"])
  , (QualificationDegreeTOEFL, map (LT.fromStrict . showt) [0 .. 120 :: Int])
  , (QualificationDegreeIELTS, map (LT.fromStrict . showt) [0, 0.5 .. 9 :: Double])]
