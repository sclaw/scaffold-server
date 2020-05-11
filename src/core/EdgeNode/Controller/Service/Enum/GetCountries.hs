{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Service.Enum.GetCountries (controller) where

import EdgeNode.Transport.Response
import qualified  EdgeNode.Transport.Error as Error
import EdgeNode.Country

import TH.Proto
import Katip
import KatipController
import qualified Data.Text as T
import Data.Aeson.WithField
import System.Directory
import Control.Monad.IO.Class
import Data.Yaml
import System.FilePath.Posix
import Control.Lens
import Data.Maybe
import Data.Bitraversable
import Data.Traversable
import BuildInfo
import Control.Lens.Iso.Extended
import Data.Functor

controller :: EdgeNodeLanguage -> KatipController (Response [WithField "enum" Country T.Text])
controller lang = do
  runTelegram $location lang
  dir <- liftIO getCurrentDirectory
  let country_dir = dir </> "enum" </> "country"
  let path = country_dir </> lang^.isoEdgeNodeLanguage <> ".yaml"
  let error e = do
        $(logTM) ErrorS (logStr (prettyPrintParseException e))
        runTelegram $location $ prettyPrintParseException e^.stext
        pure $ Error.asError @T.Text "error while getting country"
  let ok xs = do
        $(logTM) DebugS (logStr (show xs))
        for xs $ \(WithField x val_m) ->
          pure $ fmap (WithField x) val_m
  response <- liftIO $ decodeFileEither @[(WithField "enum" Country (Maybe T.Text))] path
  response' <- fmap (fmap catMaybes . fromEither) $ bitraverse error ok response
  runTelegram $location response' $> response'