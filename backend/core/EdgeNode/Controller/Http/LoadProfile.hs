{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Http.LoadProfile (controller) where

import EdgeNode.Model.User 
import EdgeNode.Error

import ReliefJsonData
import Katip
import KatipController
import qualified Data.Text as T
import Database.AutoKey
import Control.Lens
import Database.Action
import Database.Groundhog (get)
import Control.Lens.Iso.Extended

controller :: UserId -> KatipController (Alternative (Error T.Text) User)
controller uid = do
    orm <- (^.katipEnv.ormDB) `fmap` ask 
    res <- flip runTryDbConnGH orm $ do 
      profile <- get (uid^.autokey)
      $(logTM) DebugS (logStr (show profile))
      return profile
    case res of 
      Right Nothing -> return $ Error $ ResponseError "user not found"
      Right (Just x) -> return $ Fortune x
      Left e -> return $ Error $ ServerError (InternalServerError (e^.to show.stextl))