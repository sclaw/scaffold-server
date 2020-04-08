{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.User (UserApi (..), UserQualificationApi (..)) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.User

import Servant.API.Generic
import Servant.API
import Data.Aeson.WithField.Extended
import Data.Aeson.Unit
import TH.Proto
import qualified Data.Text as T

data UserApi route =
     UserApi
     { _userApiGetProfile
       :: route
       :- Description "load user's profile"
       :> "profile"
       :> Get '[JSON] (Response (OptField "image" (Id "image") Profile))
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
     , _userApiQualification
       :: route
       :- "qualification"
       :> ToServant UserQualificationApi AsApi
     } deriving stock Generic

data UserQualificationApi route =
     UserQualificationApi
     { _userQualificationApiGetDegreeTypesByCategory
       :: route
       :- Description "get degree types by given category"
       :> "category"
       :> Capture "category" EdgeNodeProviderCategory
       :> Get '[JSON] (Response [EdgeNodeQualificationDegree])
     , _userQualificationApiGetCountriesByDegreeType
       :: route
       :- Description "get countries by given qualification degree type"
       :> "category"
       :> Capture "category" EdgeNodeProviderCategory
       :> "qualification-degree"
       :> Capture "type" EdgeNodeQualificationDegree
       :> Get '[JSON] (Response [EdgeNodeCountry])
     , _userQualificationApiGetBranchesByCountry
       :: route
       :- Description "get provider's branches by given country"
       :> "category"
       :> Capture "category" EdgeNodeProviderCategory
       :> "qualification-degree"
       :> Capture "type" EdgeNodeQualificationDegree
       :> "country"
       :> Capture "country" EdgeNodeCountry
       :> Get '[JSON] (Response [WithId (Id "branch") (OnlyField "title" T.Text)])
      , _userQualificationApiGetQualificationsByBranch
       :: route
       :- Description "get qualifications"
       :> "branch"
       :> Capture "branch_id" (Id "branch")
       :> "list"
       :> Get '[JSON] (Response [WithId (Id "qualification") (OnlyField "value" T.Text)])
      , _userApiAddQualificationToTrajectory
        :: route
        :- Description "add qualififcation to trajectory"
        :> ReqBody '[JSON] [WithId (Id "qualification") AddQualification]
        :> Put '[JSON] (Response [Id "user_qualification"])
     } deriving stock Generic