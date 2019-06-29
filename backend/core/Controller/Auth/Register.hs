{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller.Auth.Register (controller) where

import           Api.User.Register.RegisterInfo
import           Api.User.Register.Response
import           Api.User.Register.ResponseErr
import           Api.User.Register.ResponseOk
import           Api.User.Register.ResponseServErr
import           Api.User.Register.Response.ResponseUnwrap
import           Model.User.Entity (UserId (..))
import qualified Api.User.Register.Error as Register

import           Katip
import           KatipController
import           Katip.Monadic
import           Network.WebSockets hiding (Response)
import           Control.Monad.IO.Class
import           Control.Lens hiding (re)
import           WebSocket
import           Text.ProtocolBuffers.Basic (defaultValue)
import           Control.Exception hiding (handle)
import           Control.Monad.Catch
import           Text.RE.PCRE.String (matched, (?=~), re)
import           Data.Validation
import           Hasql.Session
import           Hasql.Statement as HS 
import           Hasql.Encoders as HE
import           Hasql.Decoders as HD
import           Hasql.Pool
import           Control.Monad.Reader.Class
import           Database.Action
import           Control.Lens.Iso.Extended
import           Crypto.PasswordStore (pbkdf2, makePasswordSaltWith, makeSalt)
import           Data.Foldable
import           Data.Traversable
import           Text.InterpolatedString.QM

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

controller :: PendingConnection -> KatipController ()
controller pend = 
  do
    conn <- liftIO $ acceptRequest pend
    let header = pendingRequest pend
    katipAddNamespace (Namespace ["header"]) $ log InfoS header
    handle err (run conn)
  where
    run conn = 
     do 
       out :: Data RegisterInfo <- liftIO $ evaluate =<< receiveData conn
       gotLeft <- for (unwrapData out) $ \info -> do 
        katipAddNamespace (Namespace ["request"]) $ log InfoS info
        res <- traverse (const (persist info)) (validateInfo info)
        
        katipAddNamespace (Namespace ["res"]) $ 
          traverse_ (either (log ErrorS) (log InfoS)) res

        let resp = mkRespBody res 
        katipAddNamespace (Namespace ["response"]) $ do 
          log InfoS (resp :: Response)
          liftIO $ conn `sendBinaryData` Data (Right (resp :: Response))
       -- if got wrong msg 
       flip (traverseOf_ _Left) gotLeft $ \x -> 
         katipAddNamespace (Namespace ["response", "wrong"]) $
          log InfoS ("wrong req: " <> x^.from textbsliso)
    err :: SomeException -> KatipController ()  
    err e = katipAddNamespace (Namespace ["response"]) $ log CriticalS e
    log sev = $(logTM) sev . logStr . show

-- | Validate RegisterInfo
--
-- >>> validateInfo defaultValue
-- Failure [WrongEmail,PasswordWeek]
-- 
validateInfo :: RegisterInfo -> Validation [Register.Error] ()
validateInfo info  = validateEmail *> validatePassword  
  where
    emailRegex = [re|^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]{2,}$|]
    passRegex = [re|^(?=.*[A-Za-z])(?=.*\d)[A-Za-z\d]{8,}$|]
    validateEmail 
     | matched ((info^.registerInfoEmail.from sutf8) ?=~ emailRegex) = _Success # ()
     | otherwise = _Failure # [Register.WrongEmail]
    validatePassword 
     | matched ((info^.registerInfoPassword.from sutf8) ?=~ passRegex) = _Success # ()
     | otherwise = _Failure # [Register.PasswordWeek]
  
persist :: RegisterInfo -> KatipController (Either UsageError (Maybe UserId))
persist info =
  do 
    io <-katipAddNamespace (Namespace ["raw"]) askLoggerIO
    raw <- (^.katipEnv.rawDB) `fmap` ask
    flip runTryDbConnRaw raw $ do
     let sql = [qns| insert into main."User" ("userEmail", "userPassword") 
                     values ($1, $2) on conflict do nothing returning id 
               |]
     let mkSalt = makeSalt (info^.registerInfoEmail.from tutf8.textbsiso)
     let mkPass = makePasswordSaltWith pbkdf2 id 
                  (info^.registerInfoPassword.from tutf8.textbsiso) mkSalt 2000 
     let encoder = 
          ((info^.registerInfoEmail.from tutf8) >$ 
          (HE.param HE.text)) <>
          (mkPass >$ HE.param HE.bytea) 
     let decoder = HD.rowMaybe $ HD.column HD.int8 <&> (^._Unwrapped')
     liftIO $ io InfoS (logStr sql)
     statement () (HS.Statement sql encoder decoder True)

mkRespBody :: Validation [Register.Error] (Either UsageError (Maybe UserId)) -> Response
mkRespBody (Success (Right ident)) = defaultValue & responseUnwrap ?~ mkResp 
  where mkResp = maybe (Err (ResponseErr ([Register.EmailTaken]^.listSeq) Nothing)) (Ok . ResponseOk) ident
mkRespBody (Success (Left err)) = defaultValue & responseUnwrap ?~ Serv (ResponseServErr (show err^.sutf8))
mkRespBody (Failure xs) = defaultValue & responseUnwrap ?~ Err (ResponseErr (xs^.listSeq) Nothing)