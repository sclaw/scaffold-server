{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Provider
       ( ProviderApi (..)
       , PoolApi (..)
       , TagsApi (..)
       , SettingsApi (..)
       , AccountApi (..)
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.Extended
import EdgeNode.Transport.Provider.Qualification
import EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes
       (EdgeNodeQualificationDegreeCapture)
import EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries
       (EdgeNodeCountryCapture)
import EdgeNode.Transport.Provider.Pool.Tags
import EdgeNode.Transport.Provider.Settings

import Auth
import Servant.API.Generic
import Servant.API
import Data.Aeson.Unit
import Data.Aeson.WithField
import qualified Data.Text as T
import TH.Proto

data ProviderApi route =
     ProviderApi
     { _providerApiGetBranches
       :: route
       :- "branches"
       :> Get '[JSON] (Response [GetBranchResp])
     ,  _providerApiCreateBranches
       :: route
       :- "branches"
       :> ReqBody '[JSON] [MkBranchReq]
       :> Put '[JSON] (Response [Id "branch"])
     , _providerApiPatchBranches
       :: route
       :- "branches"
       :> ReqBody '[JSON] [PatchBranchReq]
       :> Patch '[JSON] (Response Unit)
     ,  _providerApiDeleteBranch
       :: route
       :- "branch"
       :> Capture "branchId" (Id "branch")
       :> Delete '[JSON] (Response Unit)
     , _providerApiSetHQ
       :: route
       :- "branch"
       :> Capture "branchId" (Id "branch")
       :> "hq"
       :> Put '[JSON] (Response Unit)
     , _providerApiPublish
       :: route
       :- "publish"
       :> Post '[JSON] (Response Unit)
     , _providerApiBuilderCreateQualification
       :: route
       :- "qualification"
       :> "builder"
       :> ReqBody '[JSON] (WithField "branch" (Id "branch") QualificationBuilder)
       :> Put '[JSON] (Response (Id "qualification"))
     , _providerApiBuilderGetAvailableBranches
       :: route
       :- "qualification"
       :> "builder"
       :> "branches"
       :> Get '[JSON] (Response [WithId (Id "branch") (OnlyField "title" T.Text)])
     , _providerApiBuilderGetDependencyAreaToCountries
       :: route
       :- "qualification"
       :> "builder"
       :> "dependency"
       :> "academic-area"
       :> Capture "area" EdgeNodeAcademicArea
       :> "countries"
       :> Get '[JSON] (Response [EdgeNodeCountryCapture])
     , _providerApiBuilderGetDependencyCountryToTypes
       :: route
       :- "qualification"
       :> "builder"
       :> "dependency"
       :> "academic-area"
       :> Capture "area" EdgeNodeAcademicArea
       :> "country"
       :> Capture "country" EdgeNodeCountry
       :> "qualification-degrees"
       :> Get '[JSON] (Response [EdgeNodeQualificationDegreeCapture])
     , _providerApiBuilderGetDependencTypeToQualifications
       :: route
       :- "qualification"
       :> "builder"
       :> "dependency"
       :> "academic-area"
       :> Capture "area" EdgeNodeAcademicArea
       :> "country"
       :> Capture "country" EdgeNodeCountry
       :> "qualification-degree"
       :> Capture "type" EdgeNodeQualificationDegree
       :> "qualifications"
       :> Get '[JSON] (Response [WithId (Id "qualification") DegreeTypeToQualification])
     , _providerApiGetQualifications
       :: route
       :- Description "qualoifications list grouped by branches"
       :> "qualification"
       :> "list"
       :> Get '[JSON] (Response [WithId (Id "qualification") ListItem])
     , _providerApiGetQualification
       :: route
       :- Description "retrieve qualififcation by its id"
       :> "qualification"
       :> Capture "qualification_id" (Id "qualification")
       :> Get '[JSON] (Response (WithId (Id "qualification") QualificationInfo))
     , _providerApiPatchQualification
       :: route
       :- Description "patch given qualififcation"
       :> "qualification"
       :> Capture "qualification_id" (Id "qualification")
       :> ReqBody '[JSON] PatchQualification
       :> Patch '[JSON] (Response Unit)
     , _providerApiPool
       :: route
       :- Description "tools for promoting qualifiation for target audience"
       :> "pool"
       :> ToServant PoolApi AsApi
     , _providerApiSettings
       :: route
       :- Description "settings"
       :> "settings"
       :> ToServant SettingsApi AsApi
     } deriving stock Generic

data PoolApi route =
     PoolApi
     { _poolApiTags
       :: route
       :- "tags"
       :> ToServant TagsApi AsApi
     } deriving stock Generic

data TagsApi route =
     TagsApi
     { _tagsApiCreate
       :: route
       :- ReqBody '[JSON] TagsBuilder
       :> Put '[JSON] (Response (Id "tags"))
     , _tagsApiGet
       :: route
       :- Capture "tags_id" (Id "tags")
       :> Get '[JSON] (Response Tags)
     , _tagsApiGetAllTags
       :: route
       :- "list"
       :> Get '[JSON] (Response TagsList)
     , _tagsApiPatch
       :: route
       :- Capture "tags_id" (Id "tags")
       :> ReqBody '[JSON] TagsPatch
       :> Patch '[JSON] (Response Unit)
     , _tagsApiPublish
       :: route
       :- Capture "tags_id" (Id "tags")
       :> "publish"
       :> Post '[JSON] (Response Unit)
     } deriving stock Generic

data SettingsApi route =
     SettingsApi
     { _settingsApiAccount
       :: route
       :- "account"
       :> ToServant AccountApi AsApi
     } deriving stock Generic

data AccountApi route =
     AccountApi
     { _accountApiCreateAccount
       :: route
       :- ReqBody '[JSON] NewAccount
       :> Put '[JSON] (Response Unit)
     , _accountApiListAccounts
       :: route
       :- "list"
       :> Get '[JSON] (Response Accounts)
     , _accountApiPatchRoles
       :: route
       :- Capture "account_id" UserId
       :> ReqBody '[JSON] AccountRoles
       :> Post '[JSON] (Response Unit)
     , _accountApiPatchPersonal
       :: route
       :- "personal"
       :> ReqBody '[JSON] PatchPersonal
       :> Patch '[JSON] (Response Unit)
     } deriving stock Generic