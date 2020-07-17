{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.User.Qualification.GetCountriesByDegreeType (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries (EdgeNodeCountryCapture (..))
import EdgeNode.Statement.User as User
import EdgeNode.Country
import qualified  EdgeNode.Transport.Error as Error

import Katip
import TH.Proto
import KatipController
import Database.Transaction
import Control.Lens
import Data.Functor
import BuildInfo
import Data.Aeson.WithField
import qualified Data.Text as T
import System.FilePath.Posix
import System.Directory
import Control.Monad.IO.Class
import Data.Yaml
import Data.Bitraversable
import Control.Lens.Iso.Extended
import Data.String.Conv
import Data.Maybe

controller :: EdgeNodeProviderCategory -> EdgeNodeQualificationDegree -> KatipController (Response [WithField "value" T.Text (OnlyField "enum" EdgeNodeCountryCapture)])
controller category degree_type = do
  runTelegram $location (category, degree_type)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  xs <- katipTransaction hasql $ statement User.getCountriesByDegreeType (category, degree_type)
  dir <- liftIO getCurrentDirectory
  let cntr_dir = dir </> "enum" </> "country"
  let path = cntr_dir </> English^.isoEdgeNodeLanguage <> ".yaml"
  let error e = do
        $(logTM) ErrorS (logStr (prettyPrintParseException e))
        runTelegram $location $ prettyPrintParseException e^.stext
        pure $ Error.asError @T.Text "error while getting qualification enums"
      ok ys = map (injectText ys) xs
  values <- liftIO $ decodeFileEither @[(WithField "enum" Country (Maybe T.Text))] path
  response <- fmap fromEither $ bitraverse error (pure . ok) values
  runTelegram $location response $> response

injectText :: [WithField "enum" Country (Maybe T.Text)] -> EdgeNodeCountryCapture -> WithField "value" T.Text (OnlyField "enum" EdgeNodeCountryCapture)
injectText xs cap@(EdgeNodeCountryCapture x) = go xs
  where go [] = error $ "unable to find " <> show x <> " at " <> show xs
        go (WithField e v:xs)
          | e^.isoCountry == x^.isoEdgeNodeCountry
            = WithField (fromMaybe (toS (show x))  v) $ OnlyField cap
          | otherwise = go xs