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
     , _providerApiCreateQualification
       :: route 
       :- "qualifiacation"
       :> "new"
       :> Put '[JSON] (Response (Id "qual"))
     } deriving stock Generic