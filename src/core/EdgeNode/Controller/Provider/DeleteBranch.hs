{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.DeleteBranch (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider ()
import EdgeNode.Transport.Id

import KatipController
import Data.Aeson.WithField ()
import Data.Aeson.Unit

controller :: Id -> Id -> KatipController (Response Unit)
controller _ _ = undefined