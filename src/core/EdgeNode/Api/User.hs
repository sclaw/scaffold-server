{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.User
       ( UserApi (..)
       , UserQualificationApi (..)
       , TrajectoryApi (..)
       , RoadmapApi (..)
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.User
import EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes (EdgeNodeQualificationDegreeCapture)
import EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries (EdgeNodeCountryCapture)
import EdgeNode.Transport.User.Roadmap

import Servant.API.Generic
import Servant.API
import Data.Aeson.WithField.Extended
import Data.Aeson.Unit
import TH.Proto
import qualified Data.Text as T
import Data.Int

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
     , _userApiTrajectory
       :: route
       :- "trajectory"
       :> ToServant TrajectoryApi AsApi
     , _userApiQualification
       :: route
       :- "qualification"
       :> ToServant UserQualificationApi AsApi
     , _userApiRoadmap
       :: route
       :- "roadmap"
       :> ToServant RoadmapApi AsApi
     } deriving stock Generic

data UserQualificationApi route =
     UserQualificationApi
     { _userQualificationApiGetDegreeTypesByCategory
       :: route
       :- Description "get degree types by given category"
       :> "category"
       :> Capture "category" EdgeNodeProviderCategory
       :> "qualification-degrees"
       :> Get '[JSON] (Response [EdgeNodeQualificationDegreeCapture])
     , _userQualificationApiGetCountriesByDegreeType
       :: route
       :- Description "get countries by given qualification degree type"
       :> "category"
       :> Capture "category" EdgeNodeProviderCategory
       :> "qualification-degree"
       :> Capture "type" EdgeNodeQualificationDegree
       :> "countries"
       :> Get '[JSON] (Response [EdgeNodeCountryCapture])
     , _userQualificationApiGetBranchesByCountry
       :: route
       :- Description "get provider's branches by given country"
       :> "category"
       :> Capture "category" EdgeNodeProviderCategory
       :> "qualification-degree"
       :> Capture "type" EdgeNodeQualificationDegree
       :> "country"
       :> Capture "country" EdgeNodeCountry
       :> "branches"
       :> Get '[JSON] (Response [WithId (Id "branch") (OnlyField "title" T.Text)])
      , _userQualificationApiGetQualificationsByBranch
       :: route
       :- Description "get qualifications"
       :> "branch"
       :> Capture "branch_id" (Id "branch")
       :> "qualification-degree"
       :> Capture "type" EdgeNodeQualificationDegree
       :> "list"
       :> Get '[JSON] (Response [WithId (Id "qualification") (OnlyField "title" T.Text)])
      , _userQualificationApiAddQualificationToTrajectory
        :: route
        :- Description "add qualififcation to trajectory"
        :> ReqBody '[JSON] [WithId (Id "qualification") AddQualificationRequest]
        :> Put '[JSON] (Response AddQualificationResponse)
      , _userQualificationApiGetQualificationList
        :: route
        :- Description "qualififcation list"
        :> "list"
        :> Get '[JSON] (Response UserQualification)
      , _userQualificationApiPurgeQualifications
        :: route
        :- Description "purge banch of qualififcations contained by provider"
        :> Capture "provider_id" (Id "provider")
        :> ReqBody '[JSON] [Id "qualification"]
        :> Delete '[JSON] (Response Int64)
      , _userQualificationApiGetSuggestions
        :: route
        :- Description "suggested qualifications (via provider pool)"
        :> "suggestion"
        :> "list"
        :> Get '[JSON] (Response QualificationSuggestions)
      , _userQualificationApiMarkSuggestion
        :: route
        :- Description "apply or reject suggestion"
        :> "suggestion"
        :> Capture "suggestion_id" (Id "suggestion")
        :> "mark"
        :> ReqBody '[JSON] QualificationSuggestion
        :> Post '[JSON] (Response Unit)
     } deriving stock Generic

data TrajectoryApi route =
     TrajectoryApi
     { _trajectoryApiAddTrajaectory
       :: route
       :- Description "add qualififcation to user's trajectories"
       :> ReqBody '[JSON] (OnlyField "id" (Id "qualification"))
       :> Put '[JSON] (Response Unit)
     , _trajectoryApiGetTrajaectories
       :: route
       :- Description "get user's trajectories"
       :> "list"
       :> Get '[JSON] (Response UserTrajectories)
     , _trajectoryApiDeleteTrajaectory
       :: route
       :- Description "purge trajectory"
       :> Capture "trajectory_id" (Id "trajectory")
       :> Delete '[JSON] (Response Unit)
     } deriving stock Generic

newtype RoadmapApi route =
        RoadmapApi
        { _roadmapApiLoad
          :: route
          :- Description "load roadmap for the given trajectory"
          :> Get '[JSON] (Response Roadmap)
        } deriving stock Generic