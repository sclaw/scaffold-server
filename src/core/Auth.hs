{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth
      ( JWTUser (..)
      , AppJwt
      , BasicUser (..)
      , BasicAuthCfgData (..)
      , UserId
      , withAuthResult
      , mkAccessToken
      , mkRefreshToken
      , applyController
      , verifyToken
      , withUser
      , mkTokens
      ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import qualified EdgeNode.Transport.Error as Transport
import EdgeNode.Model.User
import qualified EdgeNode.Transport.Auth as Auth

import Data.Time.Transport
import qualified Data.Text as T
import Servant.Auth.Server.Internal.JWT
import Servant.Auth.Server
import Control.Lens.Iso.Extended
import Control.Lens
import Servant.Auth.Server.Internal.Class
import qualified Crypto.JWT as Jose
import Data.ByteArray (constEq)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.Reader.Class
import Control.Monad
import Network.Wai (requestHeaders)
import Network.HTTP.Types.Header
import Control.Monad.Except
import Servant.Auth.Server.Internal.ConfigTypes
import KatipController
import Katip
import Data.Traversable
import Data.Bool
import Crypto.JWT
import Data.Time.Clock.System
import Data.Time.Clock
import qualified Data.HashMap.Strict as HM
import Data.Aeson hiding (Error)
import Data.Aeson.TH
import qualified Hasql.Session as Hasql
import Servant.Server
import qualified Hasql.Connection as Hasql
import qualified Data.Pool as Pool
import Database.Transaction
import Hasql.TH
import TH.Mk
import Data.Coerce
import Data.Maybe
import Servant.Auth.Swagger
import Data.Swagger hiding (HasSecurity, Response)
import Hash
import Data.Bifunctor
import System.Random.PCG.Unique
import Pretty

data AppJwt

type UserId = Id "user"

data JWTUser =
     JWTUser
     { jWTUserUserId           :: !(Id "user")
     , jWTUserEmail            :: !T.Text
     , jWTUserUserRole         :: !UserRole
     , jwtUserRefreshTokenHash :: !T.Text
     } deriving stock Show

deriveJSON defaultOptions ''JWTUser
mkEncoder ''JWTUser

mkEnumConvertor ''Auth.Error

instance FromJWT JWTUser
instance ToJWT JWTUser

instance FromJWT (Id "user")
instance ToJWT (Id "user")

instance HasSecurity AppJwt where
  securityName _ = "JWT Security"
  securityScheme _ =
   SecurityScheme
   (SecuritySchemeApiKey
    (ApiKeyParams "Authorization" ApiKeyHeader))
   (Just "JSON Web Token-based API key")

instance IsAuth AppJwt JWTUser where
  type AuthArgs AppJwt = '[JWTSettings, KatipLoggerIO, Id "user", Pool.Pool Hasql.Connection, Bool]
  runAuth _ _ cfg log uid pool =
   bool (return (JWTUser uid mempty Primary mempty))
        (Auth.jwtAuthCheck cfg log pool)

withAuthResult :: AuthResult user -> (AuthResult user -> api) -> api
withAuthResult auth api = api auth

applyController
  :: Maybe (KatipController (Response a))
  -> AuthResult JWTUser
  -> (JWTUser -> KatipController (Response a))
  -> KatipController (Response a)
applyController unauthorized user authorized =
  case user of
    Authenticated u -> authorized u
    Indefinite -> fmap (fromMaybe unauthorizedError) (sequence unauthorized)
    _ -> return unkonwnError
  where unauthorizedError = Error $ Transport.asError @T.Text $ (Auth.ErrorAuthenticationRequired^.isoError.stext)
        unkonwnError = Error $ Transport.asError @T.Text (Auth.ErrorUnknownError^.isoError.stext)

jwtAuthCheck :: JWTSettings -> KatipLoggerIO -> Pool.Pool Hasql.Connection -> AuthCheck JWTUser
jwtAuthCheck cfg logger pool = do
  headers <- fmap requestHeaders ask
  jwt <- for (getToken headers) $ \token -> do
    verified <- liftIO $ runExceptT $ verifyToken cfg token
    getJWTUser logger verified
  liftIO $ logger DebugS (logStr (mkPretty "jwt user:" jwt))
  check <- liftIO $ transaction pool logger $
    lift (actionCheckToken jwt)
  let mkErr e = do liftIO (logger ErrorS (logStr e)); mzero
  either mkErr return check

getToken :: RequestHeaders -> Maybe BS.ByteString
getToken headers = do
  authHdr <- lookup "Authorization" headers
  let bearer = "Bearer "
      (mbearer, token) =
        BS.splitAt (BS.length bearer) authHdr
  guard (mbearer `constEq` bearer)
  return token

verifyToken :: JWTSettings -> BS.ByteString -> ExceptT Jose.JWTError IO Jose.ClaimsSet
verifyToken cfg token =
  Jose.decodeCompact (BSL.fromStrict token) >>=
  Jose.verifyClaims
   (jwtSettingsToJwtValidationSettings cfg)
   (validationKeys cfg)

getJWTUser :: KatipLoggerIO -> Either Jose.JWTError Jose.ClaimsSet -> AuthCheck JWTUser
getJWTUser log (Left e) = do liftIO (log InfoS (logStr (show e))); mzero
getJWTUser log (Right claim) = either err return (decodeJWT claim)
  where err e = do liftIO (log InfoS (logStr ("decode jwt claim error " <> e))); mzero

actionCheckToken :: Maybe JWTUser -> Hasql.Session (Either String JWTUser)
actionCheckToken Nothing = return $ Left "access token not found"
actionCheckToken (Just user) = do
  let mkStatement =
        [singletonStatement|
          select exists (
            select 1
            from auth.token
            where "user_fk" = $1 :: int8
            and refresh_token_hash = $2 :: text) :: bool|]
  exists <- Hasql.statement user $
    flip lmap mkStatement $ \x ->
      ( mkEncoderJWTUser x^._1.coerced
      , mkEncoderJWTUser x^._4)
  return $ if exists then Right user else Left "refresh token not found"

mkAccessToken :: JWK -> Id "user" -> T.Text -> UserRole -> ExceptT JWTError IO (SignedJWT, Time)
mkAccessToken jwk uid refresh_token_hash utype = do
  alg <- bestJWSAlg jwk
  utc <- liftIO getCurrentTime
  let user = JWTUser uid "" utype refresh_token_hash
  let claims =
        emptyClaimsSet
        & claimIss ?~ "edgeNode"
        & claimIat ?~ NumericDate utc
        & claimExp ?~ NumericDate (addUTCTime 600 utc)
        & unregisteredClaims .~
          HM.singleton "dat" (toJSON user)
  let epoch =fromIntegral $ systemSeconds $ utcToSystemTime (addUTCTime 600 utc)
  let tm = Time epoch 0
  (,tm) <$> signClaims jwk (newJWSHeader ((), alg)) claims

mkRefreshToken :: JWK -> T.Text -> ExceptT JWTError IO SignedJWT
mkRefreshToken jwk unique = do
  alg <- bestJWSAlg jwk
  ct <- liftIO getCurrentTime
  let claims =
        emptyClaimsSet
        & claimIss ?~ "edgeNode"
        & claimIat ?~ NumericDate ct
        & claimExp ?~ NumericDate (addUTCTime (7 * 10^6) ct)
        & unregisteredClaims .~
          HM.singleton "dat" (toJSON unique)
  signClaims jwk (newJWSHeader ((), alg)) claims

data BasicUser = BasicUser { basicUserUserId :: !(Id "user") }

instance FromJWT BasicUser
instance ToJWT BasicUser

deriveJSON defaultOptions ''BasicUser

data BasicAuthCfgData =
     BasicAuthCfgData
     { basicAuthCfgDataPool   :: !(Pool.Pool Hasql.Connection)
     , basicAuthCfgDataLogger :: !KatipLoggerIO
     }

type instance BasicAuthCfg = BasicAuthCfgData

mkEncoder ''BasicAuthData

instance FromBasicAuthData BasicUser where
  fromBasicAuthData authData cfg  =
     transaction
     (basicAuthCfgDataPool cfg)
     (basicAuthCfgDataLogger cfg) $ lift $
     flip fmap checkAdmin $
     maybe
     Servant.Auth.Server.NoSuchUser
     (Authenticated . BasicUser . coerce)
    where
      checkAdmin =
        Hasql.statement
        (mkEncoderBasicAuthData authData)
        (lmap (& _1 %~ (^.from textbs))
         [maybeStatement|
           select id :: int8
           from auth.admin
           where login = $1 :: text
           and password = $2 :: bytea|])

withUser :: AuthResult user -> (user -> KatipController a) -> KatipController a
withUser user controller = do
  resp <- for user controller
  case resp of
    Authenticated resp -> return resp
    _ -> throwError err403

mkTokens :: JWK -> (Id "user", UserRole) -> KatipLoggerIO -> IO (Either T.Text (T.Text, (ByteString, Time), ByteString, T.Text))
mkTokens key cred log = do
  uq <- fmap mkHash $ uniformW64 =<< createSystemRandom
  tokens_e <- runExceptT $ do
    refresh <- mkRefreshToken key uq
    access <- mkAccessToken key (cred^._1) (mkHash refresh) (cred^._2)
    pure $ (uq, access, refresh, mkHash refresh)
  let encode x = x^.to Jose.encodeCompact.bytesLazy
  let mkError err = show err^.stext
  fmap (first mkError) $ for tokens_e $ \(uq, access, refresh, hash) -> do
    log DebugS (logStr (mkPretty "access token: " (first encode access)))
    log DebugS (logStr (mkPretty "refresh token: " (encode refresh)))
    pure (uq, first encode access, encode refresh, hash)