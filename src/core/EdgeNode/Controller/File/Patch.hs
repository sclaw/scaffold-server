{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.File.Patch (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.File as File

import KatipController
import Data.Aeson.Unit
import Servant.Multipart.File
import Control.Lens
import Database.Transaction
import Data.Int
import Data.Either.Combinators
import Data.Coerce
import Control.Lens.Iso.Extended
import Data.Traversable

controller :: Id -> File -> KatipController (Response Unit)
controller id _ = do 
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let notFound = "file {" <> show (coerce @Id @Int64 id)^.stext <> "} not found" 
  resp <- fmap (maybeToRight (asError notFound)) $ 
    katipTransaction hasql $ statement File.getHash id
  fmap fromEither $ for resp undefined