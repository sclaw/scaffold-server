{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes #-}

module EdgeNode.Transport.Iso where

import EdgeNode.Country
import EdgeNode.Lang
import EdgeNode.Transport.Qualification

import TH.Proto
import Control.Lens
import qualified Data.Text as T
import Proto3.Suite.Types
import Control.Lens.Iso.Extended

country :: Iso' (Enumerated Country) T.Text
country =  isoEnum fromCountry toCountry

language :: Iso' (Enumerated Language) T.Text
language =  isoEnum fromLanguage toLanguage

academicArea :: Iso' (Enumerated AcademicArea) T.Text
academicArea = isoEnum fromAcademicArea toAcademicArea

qualCategory :: Iso' (Enumerated QualificationCategory) T.Text
qualCategory = isoEnum fromQualificationCategory toQualificationCategory

qualStudyTime :: Iso' (Enumerated StudyTime) T.Text
qualStudyTime = isoEnum fromStudyTime toStudyTime

qualQualificationDegree :: Iso' (Enumerated QualificationDegree) T.Text
qualQualificationDegree = isoEnum fromQualificationDegree toQualificationDegree

isoEnum :: Show a => (a -> String) -> (String -> a) -> Iso' (Enumerated a) T.Text
isoEnum f t = iso fromEnum toEnum
  where fromEnum x =
          case x of 
            Enumerated (Right x) -> f x^.stext
            _ -> error $ "wrong data: " ++ show x
        toEnum x = Enumerated $ Right (t (x^.from stext))