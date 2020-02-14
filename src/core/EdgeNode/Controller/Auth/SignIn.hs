{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Auth.SignIn (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Auth

import KatipController
import Control.Lens

controller :: SigninReq -> KatipController (Response SigninResp)
controller id = do 
  _ <- fmap (^.katipEnv.hasqlDbPool) ask
  undefined