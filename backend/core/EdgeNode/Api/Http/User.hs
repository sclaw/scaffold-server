{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.User (UserApi (..)) where

import EdgeNode.Model.User

import RetrofitReqRespProto
import Servant.API.Generic
import Servant.API.WebSocket ()
import Servant.API
import ReliefJsonData
import Swagger.ToSchema ()
import qualified Data.Text as T    
import Data.Aeson.Unit

data UserApi route = 
     UserApi
     { _userApiLoadProfile
       :: route
       :- Description "load user's profile"
       :> "profile"
       :> "load" 
       :> Get '[JSON] (Alternative T.Text User)
     , _userPatchProfile
     :: route
     :- Description "load user's profile"
     :> "profile"
     :> "patch"
     :> ReqBody '[JSON] User 
     :> Patch '[JSON] (Alternative T.Text Unit)
     , _userSaveQualification
      :: route 
      :- Description "save new qualification"
      :> "qualification"
      :> "save"
      :> ReqBody '[JSON] SaveQualificationRequest
      :> Post '[JSON] (Alternative T.Text SaveQualificationResponse)
     , _userGetQualififcation
       :: route 
       :- Description "get qualification by given ids, if no ids passed all qualififcations got back"
       :> "qualification"
       :> "save"
       :> ReqBody '[JSON] GetQualificationRequest
       :> Post '[JSON] (Alternative T.Text GetQualificationResponse)                     
     } deriving stock Generic