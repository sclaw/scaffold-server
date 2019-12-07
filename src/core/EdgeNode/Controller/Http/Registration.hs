{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies #-}

module EdgeNode.Controller.Http.Registration (controller) where

import qualified EdgeNode.Api.Http.Auth.Register as Reg
import qualified EdgeNode.Model.User as User
import EdgeNode.Error

import Katip
import Katip.Core (getLoc)
import KatipController
import Pretty
import Json
import Control.Lens.Iso.Extended
import Control.Monad.IO.Class
import Text.RE.PCRE.Text (matched, (?=~), re)
import Control.Lens ((>$), (<&>), from, _Wrapped', _Just, _Unwrapped', to)
import Data.Validation
import Hasql.Session hiding (ServerError)
import qualified Hasql.Statement as HS 
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Database.Transaction
import Crypto.PasswordStore (pbkdf2, makePasswordSaltWith, makeSalt)
import Data.String.Interpolate
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product
import Data.Generics.Internal.VL.Prism
import TH.Proto
import Data.Functor (($>))
import Data.Aeson (encode)

{-
password validation:
 Minimum eight characters, at least one letter and one number: ^(?=.*[A-Za-z])(?=.*\d)[A-Za-z\d]{8,}$
 Minimum eight characters, at least one letter,
 one number and one special character: ^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{8,}$
 Minimum eight characters, at least one uppercase letter,
 one lowercase letter and one number: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d]{8,}$
 Minimum eight characters, at least one uppercase letter,
 one lowercase letter, one number and one special character: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$
 
 Minimum eight and maximum 10 characters, at least one uppercase letter,
 one lowercase letter, one number and one special character: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,10}$
email validation: ^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]{2,}$
-}

controller :: RegisterRequest -> KatipController (Alternative (Error [RegisterError]) RegisterResponse)
controller req =
  do
    $(logTM) InfoS (logStr (mkPretty "req: " (show req)))
    _ <- traverse (const (persist req)) (validateInfo req)
    undefined
      
validateInfo :: RegisterRequest -> Validation [RegisterError] ()
validateInfo info  = validateEmail *> validatePassword  
  where
    emailRegex = [re|^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]{2,}$|]
    passRegex = [re|^(?=.*[A-Za-z])(?=.*\d)[A-Za-z\d]{8,}$|]
    validateEmail 
     | matched ((info^._Wrapped'.field @"requestEmail".lazytext) ?=~ emailRegex) = _Success # ()
     | otherwise = _Failure # [WrongEmail]
    validatePassword 
     | matched ((info^._Wrapped'.field @"requestPassword".lazytext) ?=~ passRegex) = _Success # ()
     | otherwise = _Failure # [PasswordWeek]

persist :: RegisterRequest -> KatipController RegisterResponse
persist req =
  do 
    hasql <- (^.katipEnv.hasqlDbPool) `fmap` ask
    katipTransaction hasql (ask >>= lift . action)

action _ = undefined

  -- where 
  --  action logger = 
  --   do
  --     let sql = 
  --          [i| 
  --            with 
  --             cred as (insert into auth."User" 
  --              ("email", "password") 
  --              values ($1, $2) returning id),
  --             newUser as (insert into "edgeNode"."User"
  --              ("userName", "userMiddlename", "userSurname", 
  --              "userDayOfBirth", "userAllegiance", "userAvatar", "userGender"
  --              ) values
  --              ($3, $4, $5, $6, $7, $8, $9) 
  --              returning id)
  --            insert into "edgeNode"."AuthEdgeNodeUser" 
  --            ("auth", "edgeNode")
  --            values ((select id from cred), (select id from newUser))
  --            returning (select id from newUser)     
  --          |]
  --     let defu = User.defUser :: User.User      
  --     let mkSalt = makeSalt (req^._Wrapped'.field @"requestEmail".lazytext.textbs)
  --     let pass = req^._Wrapped'.field @"requestPassword".lazytext.textbs
  --     let mkPass = makePasswordSaltWith pbkdf2 id pass mkSalt 2000 
  --     let encoder = 
  --          (req^._Wrapped'.field @"requestEmail".lazytext) >$ 
  --          HE.param (HE.nonNullable HE.text) <>
  --          (mkPass >$ HE.param (HE.nonNullable HE.bytea)) <>
  --          ((defu^.field @"userName".lazytext) >$ 
  --           HE.param (HE.nonNullable HE.text)) <>
  --          ((defu^.field @"userMiddlename".lazytext) >$ 
  --           HE.param (HE.nonNullable HE.text)) <>
  --          ((defu^.field @"userSurname".lazytext) >$ 
  --           HE.param (HE.nonNullable HE.text)) <> 
  --          ((defu^?field @"userDayOfBirth"._Just.to encode.bytesLazy) >$ 
  --            HE.param (HE.nullable HE.bytea)) <>
  --          ((defu^.field @"userAllegiance".lazytext) >$ HE.param (HE.nonNullable HE.text)) <>
  --          ((defu^.field @"userAvatar") >$ HE.param (HE.nonNullable HE.bytea)) <>
  --          ((defu^.field @"userGender".to User.coercedUserGender) >$ 
  --           HE.param (HE.nonNullable (HE.enum ((^.stext) . User.fromGender))))
  --     let decoder = 
  --          HD.singleRow $ 
  --          HD.column (HD.nonNullable HD.int8) <&> 
  --          ((^._Unwrapped') . Reg.Response . Just . User.UserId)
  --     let log = (sql^.from textbs.from stext) <> ", loc: " <> show getLoc
  --     liftIO $ logger InfoS (logStr log)
  --     statement () (HS.Statement sql encoder decoder False)