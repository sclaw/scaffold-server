{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EdgeNode.Controller.Service.Enum.GetProvider (controller) where

import EdgeNode.Transport.Response
import qualified  EdgeNode.Transport.Error as Error
import EdgeNode.Transport.Provider

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

controller :: EdgeNodeLanguage -> KatipController (Response [WithField "enum" ProviderCategory T.Text])
controller lang = do
  runTelegram $location lang
  dir <- liftIO getCurrentDirectory
  let provider_dir = dir </> "enum" </> "provider"
  let path = provider_dir </> lang^.isoEdgeNodeLanguage <> ".yaml"
  let error e = do
        $(logTM) ErrorS (logStr (prettyPrintParseException e))
        runTelegram $location $ prettyPrintParseException e^.stext
        pure $ Error.asError @T.Text "error while getting provider"
  let ok xs = do
        $(logTM) DebugS (logStr (show xs))
        for xs $ \(WithField x val_m) ->
          pure $ fmap (WithField x) val_m
  response <- liftIO $ decodeFileEither @[WithField "enum" ProviderCategory (Maybe T.Text)] path
  response' <- fmap (fmap catMaybes . fromEither) $ bitraverse error ok response
  runTelegram $location response' $> response'