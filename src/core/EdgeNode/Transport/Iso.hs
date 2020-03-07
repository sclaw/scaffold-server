{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Transport.Iso where

import EdgeNode.Country
import EdgeNode.Lang
import EdgeNode.Transport.Qualification

import TH.Proto
import Control.Lens
import qualified Data.Text as T
import Proto3.Suite.Types
import Control.Lens.Iso.Extended

country :: Enum Country => Iso' (Enumerated Country) T.Text
country =  isoEnum fromCountry toCountry

language :: Enum Language => Iso' (Enumerated Language) T.Text
language =  isoEnum fromLanguage toLanguage

academicArea :: Enum AcademicArea => Iso' (Enumerated AcademicArea) T.Text
academicArea = isoEnum fromAcademicArea toAcademicArea

qualCategory :: Enum QualificationCategory => Iso' (Enumerated QualificationCategory) T.Text
qualCategory = isoEnum fromQualificationCategory toQualificationCategory

qualStudyTime :: Enum StudyTime => Iso' (Enumerated StudyTime) T.Text
qualStudyTime = isoEnum fromStudyTime toStudyTime

qualQualificationDegree :: Enum QualificationDegree => Iso' (Enumerated QualificationDegree) T.Text
qualQualificationDegree = isoEnum fromQualificationDegree toQualificationDegree

isoEnum :: (Enum a, Show a) => (a -> String) -> (String -> a) -> Iso' (Enumerated a) T.Text
isoEnum f t = iso fromEnuma toEnuma
  where fromEnuma x =
          case x of 
            Enumerated (Right x) -> f x^.stext
            Enumerated (Left _) -> f (toEnum 1)^.stext
        toEnuma x = Enumerated $ Right (t (x^.from stext))