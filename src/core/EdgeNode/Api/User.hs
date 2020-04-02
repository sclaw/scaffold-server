{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.User (UserApi (..)) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.User

import Servant.API.Generic
import Servant.API
import Data.Aeson.WithField

data UserApi route = 
     UserApi
     { _userApiGetProfile
       :: route
       :- Description "load user's profile"
       :> Get '[JSON] (Response (WithId (Id "image") Profile)) 
     } deriving stock Generic