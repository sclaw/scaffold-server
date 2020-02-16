{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Transport.ProviderExt 
       ( GetBranchResp (..)
       , MkBranchReq (..)
       ) where

import EdgeNode.Transport.Provider
import EdgeNode.Transport.Id
import GHC.Generics
import TH.Mk

data GetBranchResp = 
     GetBranchResp 
     { getBranchRespId :: !(Id "branch")
     , getBranchRespFiles :: !(Maybe [Id "file"]) 
     , getBranchRespImg :: !(Maybe (Id "img"))
     , getBranchRespIsHQ :: !Bool 
     , getBranchRespBranch :: !Branch
     } deriving (Generic, Show)

data MkBranchReq =
     MkBranchReq
     { mkBranchReqFiles :: !(Maybe [Id "file"])
     , mkBranchReqImg :: !(Maybe (Id "img"))
     , mkBranchReqBranch :: !Branch
     } deriving (Generic, Show)

mkToSchemaAndJSON ''GetBranchResp
mkToSchemaAndJSON ''MkBranchReq