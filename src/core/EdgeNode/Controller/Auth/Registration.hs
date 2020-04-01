{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Auth.Registration (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Auth
import qualified EdgeNode.Statement.Auth as Auth
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Statement.Rbac as Rbac
import EdgeNode.Model.Rbac

import Katip
import KatipController
import Data.Aeson.Unit
import Data.Aeson.WithField ()
import qualified Data.Text as T
import Database.Transaction
import Data.Bifunctor ()
import Control.Lens
import Data.Password
import Pretty
import qualified Text.RE.PCRE.Text as RegExp
import Control.Lens.Iso.Extended
import Data.Traversable

{-
password validation:
  Minimum eight characters, at least one letter and one number: ^(?=.*[A-Za-z])(?=.*\d)[A-Za-z\d]{8,}$
  Minimum eight characters, at least one letter, one number and one special character: ^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{8,}$
  Minimum eight characters, at least one uppercase letter, one lowercase letter and one number: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d]{8,}$
  Minimum eight characters, at least one uppercase letter, one lowercase letter, one number and one special character: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$
  Minimum eight and maximum 10 characters, at least one uppercase letter, one lowercase letter, one number and one special character: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,10}$
email validation - ^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]{2,}$
-}

passwordValidation password = RegExp.matched (password RegExp.?=~ [RegExp.re|^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d]{8,}$|])
emailValidation email = RegExp.matched (email RegExp.?=~ [RegExp.re|^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]{2,}$|])

controller :: Registration -> KatipController (Response Unit)
controller registeration_data | 
  registrationPassword registeration_data /= 
  registrationPasswordOnceAgain registeration_data
  = return $ Error $ Error.asError @T.Text "passwords mismatch"
controller registeration_data |
  not (passwordValidation (registrationPassword registeration_data^.lazytext))
  = return $ Error $ Error.asError @T.Text "password weak. minimum eight characters, at least one letter, one number and one special character"
controller registeration_data |
  not (emailValidation (registrationEmail registeration_data^.lazytext))
  = return $ Error $ Error.asError @T.Text "email not valid"
controller registeration_data = do
  $(logTM) DebugS (logStr (mkPretty "registeration data:" registeration_data))
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let mkResp (Just _) = Ok Unit
      mkResp Nothing = Error (Error.asError @T.Text "email taken")
  salt <-  newSalt
  fmap mkResp $ katipTransaction hasql $ do 
    ident_m <- statement (Auth.register salt) registeration_data
    for ident_m $ \x -> statement Rbac.assignRoleToUser (x, RoleUser, Nothing)