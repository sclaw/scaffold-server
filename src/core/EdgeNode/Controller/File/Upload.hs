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
import Data.Traversable
import Control.Lens.Iso.Extended
import Katip
import Control.Monad
import Hash
import Database.Transaction
import qualified Hasql.Session as Hasql
import Data.Bifunctor
import qualified Data.Text as T

controller :: EdgeNodeBucket -> File -> KatipController (Response Id)
controller bucket File {..} = do 
  Minio {..} <- fmap (^.katipEnv.minio) ask
  let hash = mkHash (fileName <> fileMime)
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
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask 
  (fromEither  . first mkErr) `fmap` (for minioResult $ const (katipTransaction hasql (ask >>= lift . action fileName fileMime hash))) 
 
mkErr :: MinioErr -> T.Text
mkErr e = show e^.stext 

action :: T.Text -> T.Text -> T.Text -> KatipLoggerIO -> Hasql.Session Id
action name mime hash _ =  Hasql.statement (name, mime, hash) save