{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.File.Upload (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.File as File
import EdgeNode.Transport.Id
import EdgeNode.Model.File

import TH.Proto
import Servant.Multipart.File
import KatipController
import Control.Lens
import Network.Minio
import Control.Monad.IO.Class
import Control.Lens.Iso.Extended
import Katip
import Control.Monad
import Hash
import Database.Transaction
import Data.Time.Clock
import Data.Traversable
import Data.Coerce
import Data.Either

controller :: EdgeNodeBucket -> Files -> KatipController (Response [Id "file"])
controller bucket x = do
  runTelegram (bucket, x)
  Minio {..} <- fmap (^.katipEnv.minio) ask
  es <- for (coerce x) $ \File {..} -> do
    tm <- liftIO getCurrentTime
    let hash = mkHash (fileName <> fileMime <> (show tm^.stext))
    minioResult <- liftIO $ runMinioWith minioConn $ do
      let newBucket =
            minioBucketPrefix <> "." <>
            (bucket^.isoEdgeNodeBucket.stext)
      exist <- bucketExists newBucket
      unless exist $
        makeBucket
        (minioBucketPrefix <> "." <>
        (bucket^.isoEdgeNodeBucket.stext))
        Nothing
      fPutObject newBucket hash filePath defaultPutObjectOptions
    $(logTM) DebugS (logStr (show minioResult))
    let tpl =
          ( Hash (UnicodeText hash)
          , Name (UnicodeText fileName)
          , Mime (UnicodeText fileMime)
          , bucket)
    return $ fmap (const tpl) minioResult
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let (errorXs, successXs) = partitionEithers es
  ids <- katipTransaction hasql $ statement File.save successXs
  return $ Warnings ids (map (asError . (\e -> show e^.stext)) errorXs)