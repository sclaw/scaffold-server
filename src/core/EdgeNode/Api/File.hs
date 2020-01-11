{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.File (FileApi (..)) where

import EdgeNode.Transport.Id

import Servant.API.Generic
import Servant.API.WebSocket ()
import Servant.API
import EdgeNode.Transport.Response
import Servant.Multipart.File
import Servant.Multipart
import TH.Proto

data FileApi route = 
     FileApi
     { _fileApiUpload
       :: route
       :- Description "upload to server"
       :> Capture "bucket" EdgeNodeBucket
       :> MultipartForm Tmp File 
       :> Post '[JSON] (Response Id)
     } deriving stock Generic