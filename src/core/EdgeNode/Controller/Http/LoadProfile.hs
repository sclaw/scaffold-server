{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Http.LoadProfile (controller) where

import EdgeNode.Model.User 
import EdgeNode.Error

import Json
import Katip
import KatipController
import qualified Data.Text as T
import Control.Lens.Iso.Extended
import Database.Action (runTryDbConnHasql)
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.Functor
import Control.Lens
import Data.String.Interpolate
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Proto3.Suite.Types

controller :: UserId -> KatipController (Alternative (Error T.Text) User)
controller uid =
  do
    hasql <- (^.katipEnv.hasqlDb) `fmap` ask
    x <- runTryDbConnHasql (action uid) hasql
    let mkErr e = 
          $(logTM) ErrorS (logStr (show e)) $> 
          Error (ServerError (InternalServerError (show e^.stextl)))
    let mkOk Nothing = return $ Error $ ResponseError "user not found"
        mkOk (Just u) = return $ Fortune u      
    either mkErr mkOk x

action :: UserId -> KatipLoggerIO -> Hasql.Session.Session (Maybe User)
action uid logger = 
  do
    let sql = 
         [i|select
              "userName",
              "userMiddlename",
              "userSurname",
              "userDayOfBirth",
              "userAllegiance",
              "userAvatar",
              "userGender" 
            from "edgeNode"."User"
            where id = $1|]
    let encoder = uid^._Wrapped' >$ HE.param (HE.nonNullable HE.int8)
    let decoder = HD.rowMaybe userDecoder
    liftIO $ logger DebugS (logStr (sql^.from textbs.from stext))
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)

userDecoder :: HD.Row User
userDecoder =
  do 
    userName <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
    userMiddlename <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
    userSurname <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
    day <- HD.column (HD.nonNullable HD.bytea) <&> Aeson.eitherDecodeStrict'  
    userAllegiance <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
    userAvatar <- HD.column (HD.nonNullable HD.bytea)
    userGender <- HD.column (HD.nonNullable (HD.enum hasqlEnumUserGender)) <&> (Enumerated . Right)
    case day of 
      Right userDayOfBirth -> return User {..}
      Left e -> error $ "json decode error: " <> e