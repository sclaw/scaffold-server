{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Provider (ProviderApi (..)) where

import EdgeNode.Transport.Provider
import EdgeNode.Transport.Id
import EdgeNode.Transport.Response

import Servant.API.Generic
import Servant.API
import Data.Aeson.WithField.Extended
import Data.Aeson.Unit

data ProviderApi route = 
     ProviderApi 
     { _providerApiGetBranches
       :: route
       :- "branch"
       :> Get '[JSON] (Response 
           [WithId (Id "branch") 
            (OptField "files" [Id "file"] 
             (OptField "image" (Id "img") (
              WithField "isHQ" Bool Branch)))])
     ,  _providerApiCreateBranches
       :: route
       :- "branch"
       :> ReqBody '[JSON] 
          [OptField "files" [Id "file"]
           (OptField "image" (Id "img") Branch)]
       :> Put '[JSON] (Response [Id "branch"])
     , _providerApiPatchBranch
       :: route
       :- "branch"
       :> Capture "branchId" (Id "branch")
       :> ReqBody '[JSON] 
          (WithField "files" [Id "file"] 
           (WithField "image" (Id "img") Branch))
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