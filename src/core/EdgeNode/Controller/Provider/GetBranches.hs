{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.GetBranches (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Id
import EdgeNode.Statement.Provider as Provider

import KatipController
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens

controller 
  :: Id "user" 
  -> KatipController 
     (Response 
      [WithId (Id "branch")
       (OptField "files" [Id "file"] 
        (OptField "image" (Id "img") 
        (WithField "isHQ" Bool Branch)))])
controller uid = 
  fmap (^.katipEnv.hasqlDbPool) ask >>= 
  (`katipTransaction` (fmap Ok (statement Provider.getBranches uid)))