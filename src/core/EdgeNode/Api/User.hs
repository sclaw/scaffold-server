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
import Data.Aeson.WithField.Extended
import Data.Aeson.Unit

data UserApi route =
     UserApi
     { _userApiGetProfile
       :: route
       :- Description "load user's profile"
       :> "profile"
       :> Get '[JSON]
          (Response
           (OptField "image"
            (Id "image") Profile))
     , _userApiPatchProfile
       :: route
       :- Description "patchuser's profile"
       :> "profile"
       :> ReqBody '[JSON]
          (OptField "image"
          (Id "image") Profile)
       :> Patch '[JSON] (Response Unit)
     , _userApiAddTrajaectory
       :: route
       :- Description "add qualififcation to user's trajectories"
       :> "trajectory"
       :> ReqBody '[JSON] (OnlyField "id" (Id "qualification"))
       :> Put '[JSON] (Response Unit)
     , _userApiAddQualification
       :: route
       :- Description "add qualififcation"
       :> "qualification"
       :> ReqBody '[JSON] [WithId (Id "qualification") AddQualification]
       :> Put '[JSON] (Response [Id "user_qualification"])
     } deriving stock Generic