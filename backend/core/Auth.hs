{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Auth (JWTUser (..), AppJwt, authGateway) where

import EdgeNode.Model.User

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

data AppJwt

data JWTUser = JWTUser { jWTUserUserId :: !UserId, jWTUserEmail :: !T.Text } 
  deriving stock Show

instance FromJWT JWTUser where
  decodeJWT _ = undefined

instance ToJWT JWTUser where
  encodeJWT _ = undefined

instance HasSecurity AppJwt where
  securityName _ = "AppJwtSecurity"
  securityScheme _ = 
   SecurityScheme 
   (SecuritySchemeApiKey 
    (ApiKeyParams "Authorization" ApiKeyHeader)) 
   (Just "JSON Web Token-based API key")
  
instance IsAuth AppJwt JWTUser where
  type AuthArgs AppJwt = '[JWTSettings, KatipLoggerIO, Bool, UserId]
  runAuth _ _ cfg log isAuth uid = bool (return (JWTUser uid mempty)) (Auth.jwtAuthCheck cfg log) isAuth

authGateway :: ThrowAll api => AuthResult JWTUser -> (JWTUser -> api) -> api
authGateway (Authenticated user) api = api user  
authGateway err _ = throwAll err401 { errBody = show err^.stext.textbsl }

jwtAuthCheck :: JWTSettings -> KatipLoggerIO -> AuthCheck JWTUser
jwtAuthCheck cfg log = 
  do
    headers <- fmap requestHeaders ask
    jwt <- for (getToken headers) $ \token -> do
     verified <- liftIO $ runExceptT $ verifyToken cfg token
     getJWTUser log verified
    let headerError = do liftIO (log InfoS "jwt header error"); mzero 
    maybe headerError return jwt

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
getJWTUser log (Right claim) =
  case decodeJWT claim of
    Left e -> do liftIO (log InfoS (logStr ("decode jwt claim error " <> e))); mzero
    Right jwtUser -> return jwtUser