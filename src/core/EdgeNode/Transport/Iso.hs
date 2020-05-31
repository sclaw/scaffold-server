{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Transport.Iso where

import EdgeNode.Country
import EdgeNode.Lang
import EdgeNode.Transport.Provider.Qualification
import EdgeNode.Transport.Provider
import EdgeNode.Transport.User
import EdgeNode.Transport.Feedback

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

providerCategory :: Enum ProviderCategory => Iso' (Enumerated ProviderCategory) T.Text
providerCategory = isoEnum fromProviderCategory toProviderCategory

qualStudyTime :: Enum StudyTime => Iso' (Enumerated StudyTime) T.Text
qualStudyTime = isoEnum fromStudyTime toStudyTime

qualQualificationDegree :: Enum QualificationDegree => Iso' (Enumerated QualificationDegree) T.Text
qualQualificationDegree = isoEnum fromQualificationDegree toQualificationDegree

currency :: Enum Currency => Iso' (Enumerated Currency) T.Text
currency = isoEnum fromCurrency toCurrency

period :: Enum Period => Iso' (Enumerated Period) T.Text
period = isoEnum fromPeriod toPeriod

gender :: Enum Gender => Iso' (Enumerated Gender) T.Text
gender = isoEnum fromGender toGender

reason :: Enum Reason => Iso' (Enumerated Reason) T.Text
reason = isoEnum fromReason toReason

isoEnum :: (Enum a, Show a) => (a -> String) -> (String -> a) -> Iso' (Enumerated a) T.Text
isoEnum f t = iso fromEnuma toEnuma
  where fromEnuma x =
          case x of
            Enumerated (Right x) -> f x^.stext
            Enumerated (Left _) -> f (toEnum 1)^.stext
        toEnuma x = Enumerated $ Right (t (x^.from stext))