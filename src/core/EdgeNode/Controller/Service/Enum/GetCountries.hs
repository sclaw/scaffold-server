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
import Pretty
import Control.Concurrent.Lifted
import Control.Monad
import Control.Lens.Iso.Extended

controller :: EdgeNodeLanguage -> KatipController (Response [WithField "enum" Country T.Text])
controller lang = do
  dir <- liftIO getCurrentDirectory
  let country_dir = dir </> "enum" </> "country"
  let path = country_dir </> lang^.isoEdgeNodeLanguage <> ".yaml"
  let error e = do
        $(logTM) ErrorS (logStr (prettyPrintParseException e))
        telegram_service <- fmap (^.katipEnv.telegram) ask
        logger <- askLoggerIO
        void $ fork $ liftIO $ send telegram_service logger (mkPretty $location (prettyPrintParseException e)^.stext)
        pure $ Error.asError @T.Text "error while getting country"
  let ok xs = do
        $(logTM) DebugS (logStr (show xs))
        for xs $ \(WithField x val_m) ->
          pure $ fmap (WithField x) val_m
  resp <- liftIO $ decodeFileEither @[(WithField "enum" Country (Maybe T.Text))] path
  fmap (fmap catMaybes . fromEither) $ bitraverse error ok resp