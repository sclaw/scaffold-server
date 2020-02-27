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

import Servant.API.Generic
import Servant.API
import Data.Aeson.Unit
import Data.Aeson.WithField
import qualified Data.Text as T

data ProviderApi route = 
     ProviderApi 
     { _providerApiGetBranches
       :: route
       :- "branch"
       :> Get '[JSON] (Response [GetBranchResp])
     ,  _providerApiCreateBranches
       :: route
       :- "branch"
       :> ReqBody '[JSON] [MkBranchReq]
       :> Put '[JSON] (Response [Id "branch"])
     , _providerApiPatchBranch
       :: route
       :- "branch"
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
       :- "qualifiacation"
       :> "builder"
       :> "create"
       :> Put '[JSON] (Response (Id "qualification"))
     , _providerApiBuilderGetAvailableBranches
       :: route 
       :- "qualifiacation"
       :> "builder"
       :> "branches" 
       :> Get '[JSON] (Response [WithId (Id "branch") (OnlyField "title" T.Text)])
     } deriving stock Generic