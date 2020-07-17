{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.User.Qualification.GetDegreeTypesByCategory (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User
import EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes (EdgeNodeQualificationDegreeCapture (..))
import EdgeNode.Controller.Service.Enum.GetQualification (Values (..), Values_QualificationDegree (..))
import qualified  EdgeNode.Transport.Error as Error
import EdgeNode.Transport.Provider.Qualification
import EdgeNode.Transport.Iso

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
import qualified Protobuf.Scalar as Protobuf
import Data.String.Conv
import Data.Coerce

controller :: EdgeNodeProviderCategory -> KatipController (Response [WithField "value" T.Text (OnlyField "enum" EdgeNodeQualificationDegreeCapture)])
controller category = do
  runTelegram $location category
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  xs <- katipTransaction hasql $ statement User.getDegreeTypesByCategory category
  dir <- liftIO getCurrentDirectory
  let qual_dir = dir </> "enum" </> "qualification"
  let path = qual_dir </> English^.isoEdgeNodeLanguage <> ".yaml"
  response <- liftIO $ decodeFileEither @Values path
  let error e = do
        $(logTM) ErrorS (logStr (prettyPrintParseException e))
        runTelegram $location $ prettyPrintParseException e^.stext
        pure $ Error.asError @T.Text "error while getting qualification enums"
      ok Values {..} = map (injectText (coerce valuesQualificationDegree)) xs
  values <- liftIO $ decodeFileEither @Values path
  response <- fmap fromEither $ bitraverse error (pure . ok) values
  runTelegram $location response $> response

injectText :: [Enums_QualificationDegreeValue] -> EdgeNodeQualificationDegreeCapture -> WithField "value" T.Text (OnlyField "enum" EdgeNodeQualificationDegreeCapture)
injectText xs cap@(EdgeNodeQualificationDegreeCapture x) = go xs
  where go [] = error $ "unable to find " <> show x <> " at " <> show xs
        go (Enums_QualificationDegreeValue e v:xs)
          | e^.qualQualificationDegree ==
            coerce cap^.isoEdgeNodeQualificationDegree.stext
            = WithField (maybe (toS (show x)) (toS . Protobuf.stringValue) v) $ OnlyField cap
          | otherwise = go xs