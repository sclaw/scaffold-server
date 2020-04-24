{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.Service.Enum.GetCountries (controller) where

import EdgeNode.Transport.Response
import qualified  EdgeNode.Transport.Error as Error
import EdgeNode.Country

import TH.Proto
import KatipController
import qualified Data.Text as T
import Data.Aeson.WithField
import System.Directory
import Control.Monad.IO.Class
import Data.Yaml
import System.FilePath.Posix
import Control.Lens
import Control.Lens.Iso.Extended
import Data.Maybe

controller :: EdgeNodeLanguage -> KatipController (Response [WithField "enum" Country T.Text])
controller country = liftIO $ do
  dir <- getCurrentDirectory
  let country_dir = dir </> "enum" </> "country"
  let path = country_dir </> country^.isoEdgeNodeLanguage <> ".yaml"
  let mkError e = Error.asError $ prettyPrintParseException e^.stext
  let mkDef (WithField x val_m) = WithField x $ fromMaybe (show x^.stext) val_m
  fmap (fromEither . bimap mkError (map mkDef)) $ decodeFileEither @[(WithField "enum" Country (Maybe T.Text))] path
