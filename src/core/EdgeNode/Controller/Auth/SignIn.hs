{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Auth.SignIn (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Auth
import qualified EdgeNode.Statement.Auth as Auth
import qualified EdgeNode.Transport.Error as Error

import KatipController
import Control.Lens
import Data.Derive.Default
import Data.DeriveTH
import Data.Default
import Default ()
import Database.Transaction
import Data.Traversable
import Data.Maybe
import qualified Data.Text as T
import Data.Password
import Data.Generics.Product.Fields
import Control.Lens.Iso.Extended

derive makeDefault ''SigninResp

controller :: SigninReq -> KatipController (Response SigninResp)
controller req = do 
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  cred <- katipTransaction hasql $ statement Auth.getUserCred req
  fmap (fromMaybe (Error (Error.asError @T.Text "credential not found"))) $ 
    for cred $ \x -> do 
      let check = checkPass (req^.field @"signinReqPassword".lazytext.to mkPass) (snd x)
      case check of 
        PassCheckSuccess -> undefined
        PassCheckFail -> pure $ Error (Error.asError @T.Text "wrong password")