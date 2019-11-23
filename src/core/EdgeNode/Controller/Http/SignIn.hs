{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.SignIn (controller) where

import EdgeNode.Model.User
import EdgeNode.Error
import EdgeNode.Api.Http.Auth.SignIn (Response (..))

import qualified Auth as Auth
import Proto
import Json
import KatipController
import Control.Lens (_Wrapped', from, (>$), (<&>), to)
import Katip
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product
import Database.Action (runTryDbConnHasql)
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Data.ByteString as B
import Control.Lens.Iso.Extended
import qualified Data.Text.Lazy as TL
import Data.String.Interpolate
import Katip.Core (getLoc)
import Control.Monad.IO.Class
import Data.Functor
import Crypto.PasswordStore (pbkdf2, verifyPasswordWith)
import Crypto.JOSE.Compact as Jose
import Data.Bifunctor
import Control.Monad.Except
import System.Random.PCG.Unique
import Hash
import Data.ByteString.UTF8
import Network.Mail.SMTP
import Control.Concurrent.Lifted
import Data.Either.Combinators
import Control.Exception

controller :: SignInRequest -> KatipController (Alternative (Error SignInError) SignInResponse)
controller req = 
  do
    let email = req^._Wrapped'.field @"requestEmail"
    let pass = req^._Wrapped'.field @"requestPassword"
    $(logTM) DebugS (logStr (email <> ", " <> pass))
    raw <- (^.katipEnv.hasqlDb) `fmap` ask
    x <- runTryDbConnHasql (actionUser email) raw
    let mkErr e = 
         $(logTM) ErrorS (logStr (show e)) $> 
         Error (ServerError (InternalServerError (show e^.stextl)))
    either mkErr (ok (pass^.lazytext.textbs)) x

actionUser :: TL.Text -> KatipLoggerIO -> Hasql.Session.Session (Maybe (UserId, B.ByteString))
actionUser email logger = 
  do
    let sql = 
         [i|select b."edgeNode"
            , u."password" 
            from "auth"."User" as u
            join "edgeNode"."AuthEdgeNodeUser" as b
            on u.id = b."auth"
            where u."email" = $1|]
    let log = (sql^.from textbs.from stext) <> ", loc: " <> show getLoc
    liftIO $ logger InfoS (logStr log)  
    let encoder = email^.lazytext >$ HE.param (HE.nonNullable HE.text)
    let decoder = 
         HD.rowMaybe $ 
          (,) <$> (HD.column (HD.nonNullable HD.int8) <&> (^.from _Wrapped'))
          <*> HD.column (HD.nonNullable HD.bytea) 
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)

ok :: B.ByteString 
   -> Maybe (UserId, B.ByteString) 
   -> KatipController 
      (Alternative 
       (Error SignInError) 
       SignInResponse)
ok _ Nothing = return $ Error (ResponseError LoginOrPaswwordWrong)
ok given (Just (_, curr)) 
  | not (verifyPasswordWith pbkdf2 id given curr) 
    = return $ Error (ResponseError LoginOrPaswwordWrong) 
ok _ (Just (uid, _)) = 
  do 
    env <- fmap (^.katipEnv) ask
    let key = env^.jwk
    let encode x = x^.to Jose.encodeCompact.bytesLazy
    unique <- liftIO $ fmap (toString . mkHash) (uniformW64 =<< createSystemRandom)
    acccess <- liftIO $ runExceptT (Auth.mkAccessToken key uid unique)
    refresh <- liftIO $ runExceptT (Auth.mkRefreshToken key uid)
    let acccesse = fmap (bimap encode Just) acccess
    let refreshe = fmap encode refresh
    -- debug
    $(logTM) DebugS (logStr ("access token: " <> show acccesse)) 
    $(logTM) DebugS (logStr ("refresh token: " <> show refreshe))
    let emailError (e :: Either SomeException ()) = 
          e `whenLeft` (($(logTM) ErrorS) . logStr . show)
    void $ forkFinally (liftIO (sendLogToEmail [i| uid: #{show uid}, token: #{show acccesse} |])) emailError
    let mkErr e = 
         $(logTM) ErrorS (logStr (show e)) $> 
         Error (ServerError (InternalServerError (show e^.stextl)))
    case (,) <$> acccesse <*> refreshe of  
     Right ((accessToken, lt), refreshToken) -> do
      x <- runTryDbConnHasql (actionToken refreshToken uid unique) (env^.hasqlDb)
      let resp (Just _) = Fortune $ SignInResponse (Response accessToken refreshToken lt)
          resp Nothing = Error (ResponseError AlreadySignedIn)      
      either mkErr (return . resp) x
     Left e -> mkErr e

sendLogToEmail txt = 
  sendMail "aspmx.l.google.com" 
  (simpleMail from to [] [] "jwt token" [plainTextPart txt])
  where from = Address Nothing "info@edgenode.org"
        to = [Address (Just "Sergey Yakovlev") "fclaw007@gmail.com"]

actionToken :: B.ByteString -> UserId -> String -> KatipLoggerIO -> Hasql.Session.Session (Maybe Bool)
actionToken token uid unique logger = 
  do
    let sql = 
         [i|insert into "auth"."Token" 
            ( "tokenRefreshToken", "tokenCreated"
            , "tokenUserId", "tokenUnique") 
            values ($1, now(), $2, $3)
            on conflict do nothing 
            returning true|]
    let log = (sql^.from textbs.from stext) <> ", loc: " <> show getLoc
    liftIO $ logger InfoS (logStr log)     
    let encoder = 
         (token >$ HE.param (HE.nonNullable HE.bytea)) <>
         (uid^._Wrapped' >$ HE.param (HE.nonNullable HE.int8)) <>
         (unique^.stext >$ HE.param (HE.nonNullable HE.text))
    let decoder = HD.rowMaybe $ HD.column (HD.nonNullable HD.bool)
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)