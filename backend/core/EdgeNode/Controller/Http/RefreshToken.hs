{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Http.RefreshToken (controller) where

import EdgeNode.Model.Token ()
import EdgeNode.Model.User (UserId)
import EdgeNode.Error

import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Database.Exception
import RetrofitReqRespProto
import ReliefJsonData
import Katip
import KatipController
import qualified Crypto.JOSE.Compact as Jose
import qualified Crypto.JWT as Jose
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product
import Control.Lens (_Wrapped', from)
import Database.Action
import Database.Groundhog ()
import qualified Data.ByteString as B
import Data.Bifunctor
import Control.Lens.Iso.Extended
import Control.Monad.Trans.Class
import Control.Monad.Error.Class (throwError, catchError)
import Control.Exception (fromException)
import Servant.Auth.Server.Internal.ConfigTypes
import qualified Data.HashMap.Strict as HM
import Data.Aeson

controller :: RefreshTokenRequest -> KatipController (Alternative (Error RefreshTokenError) RefreshTokenResponse)
controller req = 
  do 
    let bs = req^._Wrapped'.field @"requestRefreshToken"
    orm <- fmap (^.katipEnv.ormDB) ask
    key <- fmap (^.katipEnv.jwk) ask
    let logErr e = 
          do $(logTM) ErrorS (logStr (show e))
             throwError e
    let mkError e = 
         maybe 
         (ServerError (InternalServerError (show e^.stextl))) 
         (const (ResponseError RefreshTokenInvalid)) 
         (fromException e :: Maybe Groundhog) 
    (^.eitherToAlt) . first mkError <$> 
     runTryDbConnGH (action bs key `catchError` logErr) orm
   
action :: B.ByteString -> Jose.JWK -> TryAction Groundhog KatipController Postgresql RefreshTokenResponse
action bs key = 
  do 
    let cfg = defaultJWTSettings key
    signedJWT :: Jose.SignedJWT <- 
     lift $ Jose.decodeCompact (bs^.from bytesLazy)
    claims <- lift $ Jose.verifyClaims 
     (jwtSettingsToJwtValidationSettings cfg) 
     (validationKeys cfg) signedJWT
    $(logTM) DebugS (logStr ("claim: " <> show claims))
    let claimsError = JWTError . Jose.JWTClaimsSetDecodeError
    case HM.lookup "dat" (claims^.Jose.unregisteredClaims) of
      Nothing -> throwError $ claimsError "Missing 'dat' claim"
      Just v -> case fromJSON v of
        Data.Aeson.Error _ -> 
         throwError (claimsError "claim: user id decode error") 
        Success (uid :: UserId) -> 
         generateNewTokens uid
    where
      generateNewTokens _ = undefined