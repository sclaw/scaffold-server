{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Controller (controller) where

import EdgeNode.Api
import EdgeNode.Transport.Response
-- controllers
import qualified EdgeNode.Controller.File.Upload as File.Upload
import qualified EdgeNode.Controller.File.Download as File.Download
import qualified EdgeNode.Controller.File.Delete as File.Delete
import qualified EdgeNode.Controller.File.Patch as File.Patch

import Auth
import Katip
import KatipController
import Servant.Server.Generic
import Servant.API.Generic
import Control.Lens
import Database.Transaction
import Data.Bool
import qualified Data.Text as T
import BuildInfo

controller :: ApplicationApi (AsServerT KatipController)
controller = ApplicationApi { _applicationApiHttp = toServant httpApi }

httpApi :: HttpApi (AsServerT KatipController)
httpApi = HttpApi { _httpApiFile = toServant file }

file :: FileApi (AsServerT KatipController)
file =
  FileApi
  { _fileApiUpload = \bucket files ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["file", "upload"])
    (File.Upload.controller bucket files)
  , _fileApiPatch = \fid file ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["file", "patch"])
    (File.Patch.controller fid file)
  , _fileApiDelete = \fid ->
     flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["file", "delete"])
     (File.Delete.controller fid)
  , _fileApiDownload = \option fid w h ->
     flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["file", "download"])
     (File.Download.controller option fid w h)
  }