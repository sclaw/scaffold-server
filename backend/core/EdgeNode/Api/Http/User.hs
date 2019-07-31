{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.User (UserApi (..)) where

import EdgeNode.Model.User

import Servant.API.Generic
import Servant.API.WebSocket ()
import Servant.API
import ReliefJsonData
import Servant.Auth.Swagger ()
import Swagger.ToSchema ()
import RetrofitProto ()
import qualified Data.Text as T    
import Data.Aeson.Unit

data UserApi route = 
     UserApi
     { userApiLoadProfile
       :: route
       :- Description "load user's profile"
       :> "profile"
       :> Capture "uid" UserId
       :> "load" 
       :> Get '[JSON] (Alternative T.Text User)
     , userPatchProfile
     :: route
     :- Description "load user's profile"
     :> "profile"
     :> Capture "uid" UserId
     :> "patch"
     :> ReqBody '[JSON] User 
     :> Patch '[JSON] (Alternative T.Text Unit)              
     } deriving stock Generic