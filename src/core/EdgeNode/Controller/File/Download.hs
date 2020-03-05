{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module EdgeNode.Controller.File.Download (controller, Option) where

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
import Data.Aeson
import Data.Time.Clock
import Data.Text.Encoding
import qualified Data.ByteString.Base64 as B64
import TH.Mk
import Network.HTTP.Types.Header

data Option = Embedded | Raw

mkEnumConvertor ''Option
mkParamSchemaEnum ''Option [|isoOption.stext.to String|]
mkFromHttpApiDataEnum ''Option [|from stext.from isoOption.to Right|] 

controller :: Option -> Id "file" -> KatipController Application
controller option id = do
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
    case option of 
      Embedded -> embedded req resp minioResp
      Raw -> raw req resp minioResp

embedded 
  :: Request 
  -> (Response -> IO ResponseReceived)
  -> Either Error (B.ByteString, Int64, T.Text, T.Text)
  -> IO ResponseReceived
embedded req resp _ | requestMethod req /= methodGet = 
  resp $ responseLBS status200 [] $ encode $ (Response.Error (asError @T.Text "only GET allowed") :: Response.Response ())
embedded _ resp minioResp = resp $ 
  case minioResp of 
    Right minio ->
      responseLBS status200 [(hContentType, (minio^._4.textbs))] $ 
        encode (Response.Ok $ decodeUtf8 $ B64.encode (minio^._1))
    Left e -> responseLBS status200 [] $ encode $ (Response.Error e :: Response.Response ())

raw 
  :: Request 
  -> (Response -> IO ResponseReceived)
  -> Either Error (B.ByteString, Int64, T.Text, T.Text)
  -> IO ResponseReceived
raw req resp _ | requestMethod req /= methodGet = resp $ responseLBS status405 [] mempty  
raw _ resp (Right minio) = resp $ 
  responseLBS status200
  [ (hContentType, (minio^._4.textbs))
  , (hContentDisposition, 
    "attachment;filename=" <> 
    (minio^._3.textbs))] $ (minio^._1.from bytesLazy)
raw _ resp _ = resp $ responseLBS status404 [] mempty