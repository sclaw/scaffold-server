{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.File.Upload (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.File
import EdgeNode.Transport.Id

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
import qualified Hasql.Session as Hasql
import qualified Data.Text as T
import Data.Time.Clock
import Data.Traversable
import Data.Coerce
import Data.Either

controller :: EdgeNodeBucket -> Files -> KatipController (Response [Id])
controller bucket x = do 
  Minio {..} <- fmap (^.katipEnv.minio) ask
  tm <- liftIO getCurrentTime
  es <- for (coerce x) $ \File {..} -> do 
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
    return  $ fmap (const (hash, fileName, fileMime)) minioResult
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let (errorXs, successXs) = partitionEithers es
  ids <- katipTransaction hasql (lift (action successXs))
  return $ Warnings ids (map (asError .  mkErr) errorXs)

mkErr :: MinioErr -> T.Text
mkErr e = show e^.stext 

action :: [(T.Text, T.Text, T.Text)] -> Hasql.Session [Id]
action = (`Hasql.statement` save)