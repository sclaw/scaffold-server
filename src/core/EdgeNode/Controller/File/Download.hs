{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.File.Download (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Statement.File as File
import EdgeNode.Model.File
import EdgeNode.Transport.Error
import qualified EdgeNode.Transport.Response as Response

import KatipController
import Network.Wai
import qualified Data.ByteString as B
import Network.HTTP.Types
import Database.Transaction
import Control.Lens
import Data.Traversable
import Network.Minio
import Control.Monad.IO.Class
import Conduit
import qualified Data.Text as T
import Data.Either.Combinators
import Data.Coerce
import Control.Lens.Iso.Extended
import Data.Int
import Control.Monad
import Data.Bifunctor
import Data.ByteString.Builder
import Data.Aeson
import Data.Time.Clock

controller :: Id "file" -> KatipController Application
controller id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let notFound = "file {" <> show (coerce @(Id "file") @Int64 id)^.stext <> "} not found" 
  meta <- fmap (maybeToRight (asError notFound)) $ 
    katipTransaction hasql $ statement File.getMeta id
  minioResp <- fmap join $ for meta $ \x -> do 
    Minio {..} <- fmap (^.katipEnv.minio) ask
    let bucket = minioBucketPrefix <> "." <> x^._4.coerced
    r <- liftIO $ runMinioWith minioConn $ do 
      o <- getObject bucket (x^._1.coerced) defaultGetObjectOptions
      let size = oiSize (gorObjectInfo o)
      tm <- fmap show $ liftIO getCurrentTime
      path <- runConduit $ 
        gorObjectStream o .| 
        sinkSystemTempFile 
        (x^._1.coerced.from stext <> "_" <> tm)
      payload <- liftIO $ B.readFile path
      return (payload, size, x^._2.coerced @Name @_ @T.Text @_, x^._3.coerced @Mime @_ @T.Text @_)
    return $ first (asError . (\e -> show e^.stext)) r         
  return $ \req resp ->
    if requestMethod req /= 
       methodGet
    then resp $ responseLBS status200 [] $ encode $ (Response.Error (asError @T.Text "only GET allowed") :: Response.Response ())
    else
      resp $ 
        case minioResp of 
          Right x -> 
            responseLBS status200 [(hContentType, (x^._4.textbs))] $ 
            toLazyByteString $ 
            stringUtf8 "{\"success:\"" <> 
            byteString (x^._1) <> 
            stringUtf8 "}"
          Left e -> responseLBS status200 [] $ encode $ (Response.Error e :: Response.Response ())