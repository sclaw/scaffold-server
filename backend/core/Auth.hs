{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth 
      ( JWTUser (..)
      , AppJwt
      , authGateway
      , mkAccessToken
      , mkRefreshToken
      ) where

import EdgeNode.Model.User
import Time.Time

import qualified Data.Text as T
import Servant.Auth.Server.Internal.JWT
import Servant.Auth.Server
import Servant.Server.Internal.ServantErr
import Control.Lens.Iso.Extended
import Control.Lens
import Servant.Auth.Server.Internal.Class
import Servant.Auth.Swagger
import Data.Swagger hiding (HasSecurity)
import qualified Crypto.JWT as Jose
import Data.ByteArray (constEq)
import qualified Data.ByteString as BS
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
import Data.Aeson
import Data.Aeson.TH
import qualified Hasql.Pool as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.String.Interpolate
import Data.Bifunctor

data AppJwt

data JWTUser = 
     JWTUser 
     { jWTUserUserId :: !UserId
     , jWTUserEmail :: !T.Text
     , jWTUserUnique :: !String 
     } 
  deriving stock Show
  
deriveJSON defaultOptions ''JWTUser

instance FromJWT JWTUser
instance ToJWT JWTUser 

instance FromJWT UserId
instance ToJWT UserId

instance HasSecurity AppJwt where
  securityName _ = "AppJwtSecurity"
  securityScheme _ = 
   SecurityScheme 
   (SecuritySchemeApiKey 
    (ApiKeyParams "Authorization" ApiKeyHeader)) 
   (Just "JSON Web Token-based API key")
  
instance IsAuth AppJwt JWTUser where
  type AuthArgs AppJwt = '[JWTSettings, KatipLoggerIO, UserId, Hasql.Pool, Bool]
  runAuth _ _ cfg log uid pool = 
   bool (return (JWTUser uid mempty mempty)) 
        (Auth.jwtAuthCheck cfg log pool)

authGateway :: ThrowAll api => AuthResult JWTUser -> (JWTUser -> api) -> api
authGateway (Authenticated user) api = api user  
authGateway err _ = throwAll err401 { errBody = show err^.stext.textbsl }

jwtAuthCheck :: JWTSettings -> KatipLoggerIO -> Hasql.Pool -> AuthCheck JWTUser
jwtAuthCheck cfg log pool = 
  do
    headers <- fmap requestHeaders ask
    jwt <- for (getToken headers) $ \token -> do
      verified <- liftIO $ runExceptT $ verifyToken cfg token
      getJWTUser log verified
    check <- liftIO $ Hasql.use pool (actionCheckToken jwt)
    let mkErr e = do liftIO (log InfoS (logStr e)); mzero
    either mkErr return (join (first show check))
   
getToken :: RequestHeaders -> Maybe BS.ByteString
getToken headers = 
  do 
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
actionCheckToken Nothing = return $ Left "jwt header error"
actionCheckToken (Just user) = 
  do 
     let sql = 
          [i|select exists (select 1 
             from "auth"."Token" 
             where "tokenUserId" = $1 
             and "tokenUnique" = $2)|]
     let encoder = 
          (jWTUserUserId user^._Wrapped' >$ 
           HE.param HE.int8) <> 
          (jWTUserUnique user^.stext >$ 
           HE.param HE.text)
     let decoder = HD.singleRow $ HD.column HD.bool    
     exists <- Hasql.statement () (HS.Statement sql encoder decoder False)
     return $ if exists then Right user else Left "token not found"

mkAccessToken :: JWK -> UserId -> String -> ExceptT JWTError IO (SignedJWT, Time)
mkAccessToken jwk uid unique = 
  do 
     alg <- bestJWSAlg jwk
     ct <- liftIO getCurrentTime
     let user = JWTUser uid "" unique
     let claims = 
          emptyClaimsSet
          & claimIss ?~ "edgeNode"
          & claimIat ?~ NumericDate ct
          & claimExp ?~ NumericDate (addUTCTime 600 ct)
          & unregisteredClaims .~ 
            HM.singleton "dat" (toJSON user)            
     t <- liftIO getSystemTime
     let tm = Time (fromIntegral (systemSeconds t)) 0
     (,tm) <$> signClaims jwk (newJWSHeader ((), alg)) claims

mkRefreshToken :: JWK -> UserId -> ExceptT JWTError IO SignedJWT
mkRefreshToken jwk uid =
  do 
    alg <- bestJWSAlg jwk
    ct <- liftIO getCurrentTime
    let claims = 
         emptyClaimsSet
         & claimIss ?~ "edgeNode"
         & claimIat ?~ NumericDate ct
         & claimExp ?~ NumericDate (addUTCTime (7 * 10^6) ct)
         & unregisteredClaims .~ 
           HM.singleton "dat" (toJSON uid)       
    signClaims jwk (newJWSHeader ((), alg)) claims 