{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module EdgeNode.Controller.Http.PatchProfile (controller) where

import EdgeNode.Model.User
import EdgeNode.Error

import ReliefJsonData
import Katip
import KatipController
import Database.AutoKey
import Control.Lens
import Database.Action
import Database.Groundhog (replace)
import Control.Lens.Iso.Extended
import Data.Aeson.Unit

controller :: UserId -> User -> KatipController (Alternative (Error Unit) Unit)
controller uid patch = do
    $(logTM) DebugS (logStr (show patch))
    orm <- (^.katipEnv.ormDB) `fmap` ask 
    res <- flip (runTryDbConnGH :: EdgeNodeActionKatip () ()) orm $ 
      replace (uid^.autokey) patch
    return $ bimap (\x -> ServerError (InternalServerError (x^.to show.stextl))) (const Unit) res^.eitherToAlt