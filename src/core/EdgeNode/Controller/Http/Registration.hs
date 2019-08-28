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
import ReliefJsonData
import Control.Lens.Iso.Extended
import Control.Monad.IO.Class
import Text.RE.PCRE.Text (matched, (?=~), re)
import Control.Lens ((>$), (<&>), from, _Wrapped', _Just, _Unwrapped', to)
import Data.Validation
import Hasql.Session hiding (ServerError)
import qualified Hasql.Statement as HS 
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Database.Action
import Crypto.PasswordStore (pbkdf2, makePasswordSaltWith, makeSalt)
import Data.String.Interpolate
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product
import Data.Generics.Internal.VL.Prism
import RetrofitProto
import Data.Functor (($>))
import qualified Database.Exception as Exception
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
    x <- traverse (const (persist req)) (validateInfo req)
    let mkErr e = 
         $(logTM) ErrorS (logStr (show e)) $> 
         Error (ServerError (InternalServerError (show e^.stextl)))
    either mkErr return (mkResp x)
     
-- | EdgeNode.Controller.Http.Registration:validateInfo
--
-- >>> validateInfo (RegisterRequest (Reg.Request (""^.stextl) (""^.stextl)))
-- Failure [WrongEmail,PasswordWeek]
-- 
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

persist :: RegisterRequest -> KatipController (Either Exception.Hasql RegisterResponse)
persist req =
  do 
    raw <- (^.katipEnv.rawDB) `fmap` ask
    runTryDbConnHasql action raw
  where 
   action logger = 
    do
      let sql = 
           [i| 
             with 
              cred as (insert into auth."AuthenticatedUser" 
               ("authenticatedUserEmail", "authenticatedUserPassword") 
               values ($1, $2) returning id),
              newUser as (insert into "edgeNode"."User"
               ("userName", "userMiddlename", "userSurname", 
               "userDayOfBirth", "userAllegiance", "userAvatar", "userGender"
               ) values
               ($3, $4, $5, $6, $7, $8, $9) 
               returning id)
             insert into "edgeNode"."UserTablesBonds" 
             ("userTablesBondsAuth", "userTablesBondsEdgeNode") 
             values ((select id from cred), (select id from newUser))
             returning (select id from newUser)     
           |]
      let defu = User.defUser :: User.User      
      let mkSalt = makeSalt (req^._Wrapped'.field @"requestEmail".lazytext.textbs)
      let pass = req^._Wrapped'.field @"requestPassword".lazytext.textbs
      let mkPass = makePasswordSaltWith pbkdf2 id pass mkSalt 2000 
      let encoder = 
           (req^._Wrapped'.field @"requestEmail".lazytext) >$ 
           HE.param HE.text <>
           (mkPass >$ HE.param HE.bytea) <>
           ((defu^.field @"userName".lazytext) >$ 
            HE.param HE.text) <>
           ((defu^.field @"userMiddlename".lazytext) >$ 
            HE.param HE.text) <>
           ((defu^.field @"userSurname".lazytext) >$ 
            HE.param HE.text) <> 
           ((defu^?field @"userDayOfBirth"._Just.to encode.bytesLazy) >$ 
            HE.nullableParam HE.bytea) <>
           ((defu^.field @"userAllegiance".lazytext) >$ HE.param HE.text) <>
           ((defu^.field @"userAvatar") >$ HE.param HE.bytea) <>
           ((defu^.field @"userGender".to User.coercedUserGender) >$ 
            HE.param (HE.enum ((^.stext) . User.fromUserGender)))
      let decoder = 
           HD.singleRow $ 
           HD.column HD.int8 <&> 
           ((^._Unwrapped') . Reg.Response . Just . User.UserId)
      let log = (sql^.from textbs.from stext) <> ", loc: " <> show getLoc
      liftIO $ logger InfoS (logStr log)
      statement () (HS.Statement sql encoder decoder False)

-- | EdgeNode.Controller.Http.Registration:mkRespBody
--
-- >>> mkResp (Failure [PasswordWeek])
-- Right (Error (ResponseError [PasswordWeek]))
mkResp 
  :: Validation 
     [RegisterError] 
     (Either Exception.Hasql 
      RegisterResponse)
  -> Either 
     Exception.Hasql 
     (Alternative 
      (Error [RegisterError]) 
      RegisterResponse)
mkResp = validation (Right . Error . ResponseError) ok
  where ok (Left (Exception.UniqueViolation _)) 
            = Right $ Error (ResponseError [EmailTaken])
        ok (Left e) = Left e
        ok (Right ident) = Right $ Fortune ident