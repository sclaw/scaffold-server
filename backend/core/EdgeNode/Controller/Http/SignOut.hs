{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Http.SignOut (controller) where

import EdgeNode.Model.Token
import EdgeNode.Error

import Auth
import Data.Aeson.Unit
import ReliefJsonData
import Katip
import KatipController
import Database.Action
import Database.Groundhog
import Control.Lens
import Control.Lens.Iso.Extended
import Data.Either.Unwrap

controller :: JWTUser -> KatipController (Alternative (Error Unit) Unit)
controller user = 
  do
    orm <- fmap (^.katipEnv.ormDB) ask
    resp <- flip (runTryDbConnGH :: EdgeNodeActionKatip () ()) orm $ 
     delete $ 
     TokenUserIdF ==. jWTUserUserId user &&.  
     TokenUniqueF ==. (jWTUserUnique user^.stext)
    whenLeft resp $ \e -> $(logTM) ErrorS (logStr (show e))
    return $ bimap (\e -> ServerError (InternalServerError (show e^.stextl))) (const Unit) resp^.eitherToAlt