{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.File (FileApi (..)) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response

import Servant.API.Generic
import Servant.API.Extended
import Servant.Multipart.File
import Servant.Multipart
import TH.Proto
import Servant.RawM
import Data.Aeson.Unit

data FileApi route = 
     FileApi
     { _fileApiUpload
       :: route
       :- Description "upload to server"
       :> Capture "bucket" EdgeNodeBucket
       :> MultipartForm Tmp Files 
       :> Put '[JSON] (Response [Id])
     , _fileApiPatch
       :: route 
       :- Description "patch file by replacing new one"
       :> Capture "fid" Id
       :> MultipartForm Tmp File       
       :> Patch '[JSON] (Response Unit)
     , _fileApiDelete
       :: route
       :- Description "delete file"
       :> Capture "fid" Id
       :> Delete '[JSON] (Response Unit)
     , _fileApiDownload
       :: route
       :- Description "download from server"
       :> "download"
       :> Capture "fid" Id
       :> RawM
     } deriving stock Generic