{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Provider (ProviderApi (..)) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.Extended
import EdgeNode.Transport.Qualification

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
       :> "create"
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
       :> Get '[JSON] (Response [EdgeNodeCountry])
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
       :> Get '[JSON] (Response [EdgeNodeQualificationDegree])
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
     } deriving stock Generic