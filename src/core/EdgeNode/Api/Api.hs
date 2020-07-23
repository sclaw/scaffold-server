{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Api
       ( HttpApi (..)
       , module EdgeNode.Api.File
       ) where

import EdgeNode.Api.File

import Servant.API.Generic
import Servant.API
import Servant.Swagger.Tags

data HttpApi route =
     HttpApi
     {_httpApiFile
       :: route
       :- Tags "File"
       :> "file"
       :> ToServant FileApi AsApi
     } deriving stock Generic