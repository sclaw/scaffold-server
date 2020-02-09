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
           [WithId Id 
            (OptField "files" [Id] 
             (OptField "image" Id (
              WithField "isHQ" Bool Branch)))])
     ,  _providerApiCreateBranches
       :: route
       :- "branch"
       :> ReqBody '[JSON] [OptField "image" Id Branch]
       :> Post '[JSON] (Response [Id])
     , _providerApiPatchBranch
       :: route
       :- "branch"
       :> Capture "branchId" Id
       :> ReqBody '[JSON] 
          (WithField "files" [Id] 
           (WithField "image" Id Branch))
       :> Patch '[JSON] (Response Unit)
     ,  _providerApiDeleteBranch
       :: route
       :- "branch"
       :> Capture "branchId" Id
       :> Delete '[JSON] (Response Unit)      
     } deriving stock Generic