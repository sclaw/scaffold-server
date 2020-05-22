{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EdgeNode.Controller.Service.Enum.GetQualification (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Qualification
import qualified  EdgeNode.Transport.Error as Error

import Katip
import TH.Proto
import KatipController
import BuildInfo
import System.FilePath.Posix
import Control.Lens
import System.Directory
import Control.Monad.IO.Class
import Data.Yaml
import qualified Data.Text as T
import Data.Coerce
import Data.Bitraversable
import Data.Functor
import Data.Vector.Lens
import Control.Lens.Iso.Extended

data Values =
     Values
     { valuesQualificationDegree :: !Values_QualificationDegree
     , valuesStudyTime           :: !Values_StudyTime
     , valuesAcademicArea        :: !Values_AcademicArea
     , valuesPeriod              :: !Values_Period
     , valuesCurrency            :: !Values_Currency
     } deriving Show

newtype Values_QualificationDegree = Values_QualificationDegree [Enums_QualificationDegreeValue] deriving (FromJSON, Show)

newtype Values_StudyTime = Values_StudyTime [Enums_StudyTimeValue] deriving (FromJSON, Show)

newtype Values_AcademicArea = Values_AcademicArea [Enums_AcademicAreaValue] deriving (FromJSON, Show)

newtype Values_Period = Values_Period [Enums_PeriodValue] deriving (FromJSON, Show)

newtype Values_Currency = Values_Currency [Enums_CurrencyValue] deriving (FromJSON, Show)

instance FromJSON Values where
   parseJSON = withObject ($location <> ":Values") $ \o -> do
    valuesQualificationDegree <- o .: "qualificationDegree"
    valuesStudyTime <- o .: "studyTime"
    valuesAcademicArea <- o .: "academicArea"
    valuesPeriod <- o .: "period"
    valuesCurrency <- o .: "currency"
    pure Values {..}

controller :: EdgeNodeLanguage -> KatipController (Response Enums)
controller lang = do
  runTelegram $location lang
  dir <- liftIO getCurrentDirectory
  let qual_dir = dir </> "enum" </> "qualification"
  let path = qual_dir </> lang^.isoEdgeNodeLanguage <> ".yaml"
  let error e = do
        $(logTM) ErrorS (logStr (prettyPrintParseException e))
        runTelegram $location $ prettyPrintParseException e^.stext
        pure $ Error.asError @T.Text "error while getting qualification enums"
  let ok x@Values {..} = do
        $(logTM) DebugS (logStr (show x))
        pure $ Enums
          (coerce valuesQualificationDegree^.vector)
          (coerce valuesStudyTime^.vector)
          (coerce valuesAcademicArea^.vector)
          (coerce valuesPeriod^.vector)
          (coerce valuesCurrency^.vector)
  response <- liftIO $ decodeFileEither @Values path
  response' <- fmap fromEither $ bitraverse error ok response
  runTelegram $location response' $> response'