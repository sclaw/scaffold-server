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
      , BasicUser (..)
      , JWTUserEncoder
      , BasicAuthCfgData (..)
      , withAuthResult
      , mkAccessToken
      , mkRefreshToken
      , applyController
      , verifyToken
      ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response

import Data.Time
import qualified Data.Text as T
import Servant.Auth.Server.Internal.JWT
import Servant.Auth.Server
import Control.Lens.Iso.Extended
import Control.Lens
import Servant.Auth.Server.Internal.Class
import Servant.Auth.Swagger
import Data.Swagger hiding (HasSecurity, Response)
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
import Data.Aeson hiding (Error)
import Data.Aeson.TH
import qualified Hasql.Session as Hasql
import Servant.Server
import qualified Hasql.Connection as Hasql
import qualified Data.Pool as Pool
import Database.Transaction
import Hasql.TH
import TH.Mk

data AppJwt

data JWTUser = 
     JWTUser 
     { jWTUserUserId :: !Id
     , jWTUserEmail  :: !T.Text
     , jWTUserUnique :: !String 
     } 
  deriving stock Show
  
deriveJSON defaultOptions ''JWTUser
mkEncoder ''JWTUser 

instance FromJWT JWTUser
instance ToJWT JWTUser 

instance FromJWT Id
instance ToJWT Id

instance HasSecurity AppJwt where
  securityName _ = "AppJwtSecurity"
  securityScheme _ = 
   SecurityScheme 
   (SecuritySchemeApiKey 
    (ApiKeyParams "Authorization" ApiKeyHeader)) 
   (Just "JSON Web Token-based API key")
  
instance IsAuth AppJwt JWTUser where
  type AuthArgs AppJwt = '[JWTSettings, KatipLoggerIO, Id, Pool.Pool Hasql.Connection, Bool]
  runAuth _ _ cfg log uid pool = 
   bool (return (JWTUser uid mempty mempty)) 
        (Auth.jwtAuthCheck cfg log pool)

withAuthResult :: AuthResult JWTUser -> (AuthResult JWTUser -> api) -> api
withAuthResult auth api = api auth

applyController 
  :: Maybe (KatipController (Response a)) 
  -> AuthResult JWTUser
  -> (JWTUser -> KatipController (Response a)) 
  -> KatipController (Response a)
applyController unauthorized user authorized = 
  case user of 
    Authenticated u -> authorized u
    Indefinite -> do 
      out <- sequence unauthorized  
      maybe (throwAll err401) return out
    err -> return $ Error undefined

jwtAuthCheck :: JWTSettings -> KatipLoggerIO -> Pool.Pool Hasql.Connection -> AuthCheck JWTUser
jwtAuthCheck cfg logger pool = 
  do
    headers <- fmap requestHeaders ask
    jwt <- for (getToken headers) $ \token -> do
      verified <- liftIO $ runExceptT $ verifyToken cfg token  
      getJWTUser logger verified
    check <- liftIO $ transaction pool logger $ lift (actionCheckToken jwt)
    let mkErr e = do liftIO (logger InfoS (logStr e)); mzero
    either mkErr return check
   
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
     let mkStatement =
          [singletonStatement|
            select exists 
            (select 1 
             from auth.token 
             where user = $1 :: int8 
             and uid = $2 :: text) :: bool|]    
     exists <- Hasql.statement user $ 
       flip lmap mkStatement $ \x -> 
         (mkEncoderJWTUser x^._1.coerced, mkEncoderJWTUser x^._3.stext)   
     return $ if exists then Right user else Left "token not found"

mkAccessToken :: JWK -> Id -> String -> ExceptT JWTError IO (SignedJWT, Time)
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

mkRefreshToken :: JWK -> Id -> ExceptT JWTError IO SignedJWT
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


data BasicUser = BasicUser { basicUserUserId :: !Id }

instance FromJWT BasicUser
instance ToJWT BasicUser

deriveJSON defaultOptions ''BasicUser

data BasicAuthCfgData = 
     BasicAuthCfgData 
     { basicAuthCfgDataPool   :: !(Pool.Pool Hasql.Connection)
     , basicAuthCfgDataLogger :: !KatipLoggerIO 
     }

type instance BasicAuthCfg = BasicAuthCfgData

instance FromBasicAuthData BasicUser where
  fromBasicAuthData _ cfg  = 
    transaction 
    (basicAuthCfgDataPool cfg) 
    (basicAuthCfgDataLogger cfg) $ 
    lift $ checkAdmin
    where checkAdmin = undefined