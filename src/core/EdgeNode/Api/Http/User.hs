{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.User (UserApi (..)) where

import EdgeNode.Model.User
import EdgeNode.User.Qualification
import EdgeNode.Model.User.Qualification ()
import EdgeNode.Provider.Qualification
import EdgeNode.Model.Provider

import RetrofitProto
import Servant.API.Generic
import Servant.API.WebSocket ()
import Servant.API
import ReliefJsonData
import Data.Aeson.Unit
import qualified Data.Text as T
import Data.Aeson.WithField
import Data.Int
import Data.Word

data UserApi route = 
     UserApi
     { _userApiLoadProfile
       :: route
       :- Description "load user's profile"
       :> "profile"
       :> "load" 
       :> Get '[JSON] (Alternative (Error T.Text) User)
     , _userPatchProfile
     :: route
     :- Description "load user's profile"
     :> "profile"
     :> "patch"
     :> ReqBody '[JSON] User 
     :> Patch '[JSON] (Alternative (Error T.Text) Unit)
     , _userSaveQualifications
      :: route 
      :- Description "save new qualification"
      :> "qualification"
      :> "save"
      :> ReqBody '[JSON] SaveQualificationsRequest
      :> Put '[JSON] (Alternative (Error T.Text) SaveQualificationsResponse)
     , _userGetFullInfoQualififcation
       :: route 
       :- Description "get qualification by given ids, if no ids passed all qualififcations got back"
       :> "qualification"
       :> "get"
       :> QueryParam "id" UserQualificationId
       :> Get '[JSON] (Alternative (Error T.Text) GetQualificationFullInfoResponse)
     , _userGetCategories
       :: route 
       :- Description "list of categories"
       :> "qualification"
       :> "new"
       :> "get" 
       :> "categories"
       :> Get '[JSON] (Alternative (Error Unit) GetCategoriesResponse)
     , _userGetProvider
       :: route 
       :- Description "list of provider"
       :> "qualification"
       :> "new"
       :> "get" 
       :> "providers"
       :> Capture "country" WrapperCountry
       :> Capture "category" WrapperType
       :> Capture "id" Int64
       :> QueryParam "cursor" Word32
       :> Get '[JSON] (Alternative (Error T.Text) GetProvidersResponse)
     , _userGetQualififcations  
       :: route 
       :- Description "list of provider"
       :> "qualification"
       :> "new"
       :> "get" 
       :> "qualififcations"
       :> Capture "id" ProviderId
       :> Get '[JSON] (Alternative (Error T.Text) GetQualififcationsResponse)
     , _userGetTrajectories
       :: route 
        :- Description "list of trajectories"
        :> "trajectory"
        :> "list"
        :> Get '[JSON] (Alternative (Error T.Text) GetTrajectoriesResponse)
     , _userSaveTrajectory
       :: route 
       :- Description "add trajectory to wish list"
       :> "trajectory"
       :> "add"
       :> ReqBody '[JSON] SaveTrajectoryRequest
       :> Put '[JSON] 
          (Alternative (Error [WithField "qualificationId" (Maybe QualificationId) SaveTrajectoryError]) SaveTrajectoryResponse)                 
     } deriving stock Generic