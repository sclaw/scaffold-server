{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Http.PatchProfile (controller) where

import EdgeNode.Model.User

import ReliefJsonData
import Katip
import KatipController
import qualified Data.Text as T
import Database.AutoKey
import Control.Lens
import Database.Action
import Control.Monad.Reader.Class
import Database.Groundhog (replace)
import Control.Lens.Iso.Extended
import Data.Aeson.Unit

controller :: UserId -> User -> KatipController (Alternative T.Text Unit)
controller uid patch = do
    $(logTM) DebugS (logStr (show patch))
    orm <- (^.katipEnv.ormDB) `fmap` ask 
    res <- flip runTryDbConnGH orm $ 
      replace (uid^.autokey) patch
    return $ bimap (^.to show.stext) (const Unit) res^.eitherToAlt