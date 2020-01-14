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
       :> MultipartForm Tmp Files 
       :> Post '[JSON] (Response [Id])
     , _fileApiPatch
       :: route 
       :- Description ""
       :> Patch '[JSON] ()
     , _fileApiDelete
       :: route
       :- Description ""
       :> Delete '[JSON] ()
     , _fileApiDownload
       :: route
       :- Description ""
       :> Post '[JSON] ()
     , _fileApiPreview
       :: route
       :- Description ""
       :> Post '[JSON] ()
     } deriving stock Generic