{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module EdgeNode.Transport.Extended 
       ( GetBranchResp (..)
       , MkBranchReq (..)
       , PatchBranchReq (..)
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

data PatchBranchReq = 
     PatchBranchReq
     { patchBranchReqId :: !(Id "branch")
     , patchBranchReqFiles :: !(Maybe [Id "file"])
     , patchBranchReqImg :: !(Maybe (Id "img"))
     , patchBranchReqBranch :: !Branch 
     } deriving (Generic, Show)


mkToSchemaAndJSON ''GetBranchResp
mkToSchemaAndJSON ''MkBranchReq
mkToSchemaAndJSON ''PatchBranchReq
