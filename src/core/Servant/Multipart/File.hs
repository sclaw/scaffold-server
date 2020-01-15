{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Multipart.File (Files (..), File (..)) where

import qualified Data.Text as T
import Servant.Swagger
import qualified Servant.Swagger.Internal
import Data.Proxy
import Servant.API
import Servant.Multipart
import Data.Swagger as Swagger
import Control.Lens

newtype Files = Files { filesXs :: [File] }

data File = 
     File
     { fileName :: !T.Text
     , fileMime :: !T.Text
     , filePath :: !FilePath
     }

instance FromMultipart Tmp Files where 
    fromMultipart x = 
      Just $ Files $ flip map (files x) $ \FileData {..} -> 
        File fdFileName fdFileCType fdPayload

instance HasSwagger sub => HasSwagger (MultipartForm tag Files :> sub) where
  toSwagger _ = 
    toSwagger (Proxy :: Proxy sub) & 
    Servant.Swagger.Internal.addParam paramFile
    where
      paramFile = 
        mempty
        & Swagger.name .~ "payload"
        & Swagger.required ?~ True
        & Swagger.schema .~ 
          Swagger.ParamOther
          (mempty
           & Swagger.in_ .~ Swagger.ParamFormData
           & Swagger.paramSchema .~ 
             (mempty & Swagger.type_ .~ 
             Swagger.SwaggerFile))