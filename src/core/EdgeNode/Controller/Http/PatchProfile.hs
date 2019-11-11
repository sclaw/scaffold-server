{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Http.PatchProfile (controller) where

import EdgeNode.Model.User
import EdgeNode.Error

import Json
import Katip
import KatipController
import Control.Lens
import Database.Action
import Control.Lens.Iso.Extended
import Data.Aeson.Unit
import qualified Data.Text as T
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.Functor
import Data.String.Interpolate
import Control.Monad.IO.Class
import Data.Int
import qualified Data.Aeson as  Aeson

controller :: User -> UserId -> KatipController (Alternative (Error T.Text) Unit)
controller new uid =
  do
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- runTryDbConnHasql (action uid new) raw
    let mkErr e = 
          $(logTM) ErrorS (logStr (show e)) $> 
          Error (ServerError (InternalServerError (show e^.stextl)))
    let mkOk 0 = return $ Error $ ResponseError "user not found"
        mkOk 1 = return $ Fortune Unit
        mkOk _ = return $ Error $ ServerError (InternalServerError "user ambiguous")      
    either mkErr mkOk x

action :: UserId -> User -> KatipLoggerIO -> Hasql.Session.Session Int64
action uid user logger = 
  do
    let sql = 
          [i|update "edgeNode"."User" set
              "userName" = $2,
              "userMiddlename" = $3,
              "userSurname" = $4,
              "userDayOfBirth" = $5,
              "userAllegiance" = $6,
              "userAvatar" = $7,
              "userGender" = $8 
             where id = $1|]
    let encoder = 
         (uid^._Wrapped' >$ HE.param HE.int8) <>
         encoderUser user
    let decoder = HD.rowsAffected
    liftIO $ logger DebugS (logStr (sql^.from textbs.from stext))
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)

encoderUser :: User -> HE.Params ()
encoderUser user = 
  ((userName user^.lazytext) >$ HE.param HE.text) <>
  ((userMiddlename user^.lazytext) >$ HE.param HE.text) <>
  ((userSurname user^.lazytext) >$ HE.param HE.text) <>
  (Aeson.encode (userDayOfBirth user)^.bytesLazy >$ HE.param HE.bytea) <>
  (userAllegiance user^.lazytext >$ HE.param HE.text) <>
  (userAvatar user >$ HE.param HE.bytea) <>
  (coercedUserGender (userGender user) >$ HE.param (HE.enum (^.isoGender.stext)))