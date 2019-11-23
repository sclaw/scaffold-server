{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.SignOut (controller) where

import EdgeNode.Error

import Auth
import Data.Aeson.Unit
import Json
import Katip
import KatipController
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Control.Lens
import Control.Lens.Iso.Extended
import qualified Data.Text as T
import Database.Action (runTryDbConnHasql)
import Data.Functor
import Data.String.Interpolate
import Control.Monad.IO.Class
import Data.Int

controller :: JWTUser -> KatipController (Alternative (Error T.Text) Unit)
controller user =  
  do 
    raw <- (^.katipEnv.hasqlDb) `fmap` ask
    x <- runTryDbConnHasql (action user) raw
    let mkErr e = 
         $(logTM) ErrorS (logStr (show e)) $> 
         Error (ServerError (InternalServerError (show e^.stextl)))
    let mkOk i 
         | i == 0 = 
            $(logTM) InfoS (logStr ("no match token with user " <> show user)) $> 
            Fortune Unit
         | i == 1 = return $ Fortune Unit
         | otherwise = 
            $(logTM) InfoS (logStr ("multiple match token with user " <> show user)) $> 
            Fortune Unit     
    either mkErr mkOk x
    
action :: JWTUser -> KatipLoggerIO -> Hasql.Session.Session Int64
action user logger = 
  do
    let sql = 
         [i|delete from "auth"."Token"  
            where "tokenUserId" = $1 and 
                  "tokenUnique" = $2|]
    let encoder = 
         (jWTUserUserId user^._Wrapped' >$ HE.param (HE.nonNullable HE.int8)) <>
         (jWTUserUnique user^.stext >$ HE.param (HE.nonNullable HE.text))
    let decoder = HD.rowsAffected
    liftIO $ logger DebugS (logStr (sql^.from textbs.from stext))
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)