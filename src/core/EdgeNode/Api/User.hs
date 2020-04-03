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
import Data.Aeson.Unit

data UserApi route = 
     UserApi
     { _userApiGetProfile
       :: route
       :- Description "load user's profile"
       :> Get '[JSON] (Response (WithId (Maybe (Id "image")) Profile))
     , _userApiPatchProfile
       :: route
       :- Description "patchuser's profile"
       :> ReqBody '[JSON] (WithId (Maybe (Id "image")) Profile)
       :> Patch '[JSON] (Response Unit)   
     } deriving stock Generic