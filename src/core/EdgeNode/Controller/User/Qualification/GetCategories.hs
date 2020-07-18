{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.User.Qualification.GetCategories (controller, EdgeNodeCategoryCapture (..)) where

import EdgeNode.Transport.Response
import qualified  EdgeNode.Transport.Error as Error
import EdgeNode.Transport.Provider

import KatipController
import Control.Lens
import Katip
import TH.Proto
import GHC.Generics hiding (from, to)
import Data.Aeson
import Control.Lens.Iso.Extended
import Data.Swagger.Internal.Schema
import Data.Proxy
import Data.Swagger hiding (Response)
import Data.Aeson.WithField
import qualified Data.Text as T
import System.FilePath.Posix
import System.Directory
import Control.Monad.IO.Class
import Data.Yaml
import Data.Bitraversable
import Data.String.Conv
import Data.Maybe
import BuildInfo
import Data.Functor
import Data.Traversable

newtype EdgeNodeCategoryCapture = EdgeNodeCategoryCapture EdgeNodeProviderCategory
  deriving newtype Generic
  deriving stock Show

instance ToSchema EdgeNodeCategoryCapture where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    return $ NamedSchema (Just "EdgeNodeCategoryCapture") $ mempty
      & type_ ?~ SwaggerString
      & enum_ ?~ map (^.isoEdgeNodeProviderCategory.stext.to String) [StateExam .. None]

instance ToJSON EdgeNodeCategoryCapture where
  toJSON x = String $ x^.coerced.isoEdgeNodeProviderCategory.stext

data Category = Category EdgeNodeCategoryCapture T.Text deriving stock Show

instance FromJSON Category where
  parseJSON = withObject "EdgeNodeCategoryCapture" $ \o -> do
    enum <- o .: "enum"
    value_m <- o .:? "value"
    let x = (enum :: ProviderCategory)^.isoProviderCategory.from isoEdgeNodeProviderCategory
    pure $ Category (EdgeNodeCategoryCapture x) (fromMaybe (toS (show x)) value_m)

controller :: KatipController (Response [WithField "value" T.Text (OnlyField "enum" EdgeNodeCategoryCapture)])
controller = do
  runTelegram $location English
  dir <- liftIO getCurrentDirectory
  let provider_dir = dir </> "enum" </> "provider"
  let path = provider_dir </> English^.isoEdgeNodeLanguage <> ".yaml"
  let error e = do
        $(logTM) ErrorS (logStr (prettyPrintParseException e))
        runTelegram $location $ prettyPrintParseException e^.stext
        pure $ Error.asError @T.Text "error while getting provider"
  let ok xs = do
        $(logTM) DebugS (logStr (show xs))
        for xs $ \(Category enum value) -> pure $ WithField value $ OnlyField enum
  response <- liftIO $ decodeFileEither @[Category] path
  response' <- fmap fromEither $ bitraverse error ok response
  runTelegram $location response' $> response'