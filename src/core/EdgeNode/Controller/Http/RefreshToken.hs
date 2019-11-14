{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.RefreshToken (controller) where

import EdgeNode.Model.User (UserId)
import EdgeNode.Error
import EdgeNode.Api.Http.Auth.RefreshToken (Response (..))

import Auth
import Proto
import Json
import Katip
import KatipController
import qualified Crypto.JOSE.Compact as Jose
import qualified Crypto.JWT as Jose
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product
import Control.Lens (_Wrapped', to, (>$))
import Database.Action
import Database.Groundhog ()
import qualified Data.ByteString as B
import Data.Bifunctor
import Control.Lens.Iso.Extended
import Control.Monad.IO.Class
import Servant.Auth.Server.Internal.ConfigTypes
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import System.Random.PCG.Unique
import Hash
import Data.ByteString.UTF8
import Control.Monad.Except
import Data.Time.Clock.System
import Time.Time
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Data.Text as T
import Data.String.Interpolate
import Data.Functor

controller :: RefreshTokenRequest -> KatipController (Alternative (Error RefreshTokenError) RefreshTokenResponse)
controller req = 
  do 
    let bs = req^._Wrapped'.field @"requestRefreshToken"
    raw <- (^.katipEnv.rawDB) `fmap` ask
    key <- fmap (^.katipEnv.jwk) ask
    x <- runTryDbConnHasql (action bs key) raw
    let mkError e =
         $(logTM) ErrorS (logStr (show e)) $> 
         Json.Error (ServerError (InternalServerError (show e^.stextl)))
    let mkOk (Left e) = 
          $(logTM) ErrorS (logStr e) $> 
          Json.Error (ResponseError RefreshTokenInvalid)
        mkOk (Right resp) = return $ Fortune resp
    either mkError mkOk x

action :: B.ByteString -> Jose.JWK -> KatipLoggerIO -> Hasql.Session.Session (Either T.Text RefreshTokenResponse)
action bs key logger =
  do 
    let cfg = defaultJWTSettings key
    e <- liftIO $ runExceptT $ verifyToken cfg bs
    case e of 
      Left e -> return $ Left (show e^.stext)
      Right claims ->
        case HM.lookup "dat" (claims^.Jose.unregisteredClaims) of
          Nothing -> return $ Left "Missing 'dat' claim"
          Just v -> case fromJSON v of
            Data.Aeson.Error _ -> return $ Left "claim: user id decode error" 
            Success (uid :: UserId) -> generateNewTokens key uid bs logger 
 
generateNewTokens 
  :: Jose.JWK 
  -> UserId 
  -> B.ByteString 
  -> KatipLoggerIO 
  -> Hasql.Session.Session (Either T.Text RefreshTokenResponse)
generateNewTokens key uid old logger =
  do       
    unique <- liftIO $ fmap (toString . mkHash) (uniformW64 =<< createSystemRandom)   
    acccess <- liftIO $ runExceptT (mkAccessToken key uid unique)
    refresh <- liftIO $ runExceptT (mkRefreshToken key uid)
    let encode x = x^.to Jose.encodeCompact.bytesLazy
    let acccesse = fmap (first encode) acccess
    let refreshe = fmap encode refresh
    liftIO $ logger DebugS (logStr ("access token: " <> show acccesse)) 
    liftIO $ logger DebugS (logStr ("refresh token: " <> show refreshe))
    case (,) <$> acccesse <*> refreshe of  
      Right ((accessToken, lt), new) ->
        do let utc = systemToUTCTime $ MkSystemTime (fromIntegral (timeEpoch lt)) 0
           let sql = 
                [i|update "auth"."Token" 
                   set "tokenRefreshToken" = $4,
                       "tokenCreated" = $5,
                       "tokenUnique" = $2
                   where "tokenUserId" = $1 and 
                         "tokenRefreshToken" = $3|]
           let encoder =
                (uid^._Wrapped' >$ HE.param (HE.nonNullable HE.int8)) <>
                (unique^.stext >$ HE.param (HE.nonNullable HE.text)) <>
                (old >$ HE.param (HE.nonNullable HE.bytea)) <>
                (new >$ HE.param (HE.nonNullable HE.bytea)) <>
                (utc >$ HE.param (HE.nonNullable HE.timestamptz))
           let decoder = HD.noResult
           Hasql.Session.statement () (HS.Statement sql encoder decoder False)
           return $ Right $  
             RefreshTokenResponse 
             (Response new accessToken (Just lt))
      Left e -> return $ Left (show e^.stext)