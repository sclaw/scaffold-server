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
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module EdgeNode.Controller.Http.Registration (controller) where

import qualified EdgeNode.Api.Http.Auth.Register as Auth
import qualified EdgeNode.Model.User.Entity as User

import Katip
import Katip.Core (getLoc)
import KatipController
import Pretty
import ReliefJsonData
import Control.Lens.Iso.Extended
import Control.Monad.IO.Class
import Text.RE.PCRE.Text (matched, (?=~), re)
import Katip.Monadic
import Control.Lens ((>$), (<&>), from, _Left)
import Data.Validation
import Hasql.Session
import qualified Hasql.Statement as HS 
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Hasql.Pool
import Control.Monad.Reader.Class
import Database.Action
import Crypto.PasswordStore (pbkdf2, makePasswordSaltWith, makeSalt)
import Text.InterpolatedString.QM
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product
import Data.Generics.Internal.VL.Prism
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as LT
import Data.Bifunctor
import RetrofitProto
import Data.Functor (($>))
import qualified Database.Exception as Exception

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

controller :: Auth.RegisterInfo -> KatipController (Alternative [ErrorReg] User.UserIdWrapper)
controller info = 
  do
    $(logTM) InfoS (logStr (mkPretty "registration info: " (show info)))
    x <- traverse (const (persist info)) (validateInfo info)
    let mkErr e = $(logTM) ErrorS (logStr (show e)) $> Error [InternalServerError]
    either mkErr return (mkResp x)
     
-- | EdgeNode.Controller.Http.Registration:validateInfo
--
-- >>> validateInfo (Auth.RegisterInfo (""^.stextl) (""^.stextl))
-- Failure [WrongEmail,PasswordWeek]
-- 
validateInfo :: Auth.RegisterInfo -> Validation [ErrorReg] ()
validateInfo info  = validateEmail *> validatePassword  
  where
    emailRegex = [re|^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]{2,}$|]
    passRegex = [re|^(?=.*[A-Za-z])(?=.*\d)[A-Za-z\d]{8,}$|]
    validateEmail 
     | matched ((info^.field @"registerInfoEmail".lazytext) ?=~ emailRegex) = _Success # ()
     | otherwise = _Failure # [WrongEmail]
    validatePassword 
     | matched ((info^.field @"registerInfoPassword".lazytext) ?=~ passRegex) = _Success # ()
     | otherwise = _Failure # [PasswordWeek]

persist :: Auth.RegisterInfo -> KatipController (Either Exception.Hasql User.UserIdWrapper)
persist info =
  do 
    raw <- (^.katipEnv.rawDB) `fmap` ask
    runTryDbConnHasql action raw
  where 
   action logger = 
    do
      let sql = 
           [qns| insert into main."User" ("userEmail", "userPassword") 
                 values ($1, $2) returning id 
           |]
      let mkSalt = makeSalt (info^.field @"registerInfoEmail".lazytext.textbs)
      let pass = info^.field @"registerInfoPassword".lazytext.textbs
      let mkPass = makePasswordSaltWith pbkdf2 id pass mkSalt 2000 
      let encoder = 
           (info^.field @"registerInfoEmail".lazytext) >$ 
           HE.param HE.text <>
           (mkPass >$ HE.param HE.bytea) 
      let decoder = HD.singleRow $ HD.column HD.int8 <&> User.wrapId
      let log = (sql^.from textbs.from stext) <> ", loc: " <> show getLoc
      liftIO $ logger InfoS (logStr log)
      statement () (HS.Statement sql encoder decoder True)

-- | EdgeNode.Controller.Http.Registration:mkRespBody
--
-- >>> mkResp (Failure [PasswordWeek])
-- Right (Error [PasswordWeek])
--    
-- >>> mkResp (Success (Right (User.wrapId 1)))
-- Right (Fortune (UserIdWrapper {unwrap = UserId {userIdUserIdIdent = 1}}))
mkResp 
  :: Validation 
     [ErrorReg] 
     (Either Exception.Hasql 
      User.UserIdWrapper)
  -> Either 
     Exception.Hasql 
     (Alternative 
      [ErrorReg] User.UserIdWrapper)
mkResp = validation (Right . Error) ok
  where ok (Left (Exception.UniqueViolation _)) 
            = Right $ Error [EmailTaken]
        ok (Left e) = Left e
        ok (Right ident) = Right $ Fortune ident