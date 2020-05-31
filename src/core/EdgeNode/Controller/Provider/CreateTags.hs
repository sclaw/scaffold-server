{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.CreateTags (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider.Pool.Tags
import EdgeNode.Transport.Id

import Auth
import KatipController

controller :: TagsBuilder -> UserId -> KatipController (Response (Id "tags"))
controller _ _ = undefined