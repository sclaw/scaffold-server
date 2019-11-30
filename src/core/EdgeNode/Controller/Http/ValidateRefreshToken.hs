{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Http.ValidateRefreshToken (controller) where

import EdgeNode.Api.Http.Auth.RefreshTokenValidator

import TH.Proto
import Auth
import Json
import KatipController
import qualified Data.Text as T
import Data.Generics.Product
import Control.Lens
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Servant.Auth.Server
import Proto3.Suite.Types
import Crypto.JWT (JWTError (..))
import Control.Lens.Iso.Extended
import qualified Crypto.JWT as Jose

controller :: RefreshTokenValidatorRequest -> KatipController (Alternative (Error T.Text) RefreshTokenValidatorResponse)
controller req = do
  let token = req^._Wrapped'.field @"requestRefreshToken" 
  jw <- (^.katipEnv.jwk) `fmap` ask
  e <- liftIO $ runExceptT $ verifyToken (defaultJWTSettings jw) token
  return $ handleValidation e

handleValidation :: Either JWTError Jose.ClaimsSet -> (Alternative (Error T.Text) RefreshTokenValidatorResponse)
handleValidation (Right _) = 
      Fortune $ 
      RefreshTokenValidatorResponse $ 
      Response $ 
      Enumerated $ 
      Right StatusOk
handleValidation (Left (JWSError e)) = Error $ ResponseError $ show e^.stext
handleValidation (Left (JWTClaimsSetDecodeError e)) = Error $ ResponseError $ e^.stext
handleValidation _ = 
  Fortune $ 
  RefreshTokenValidatorResponse $ 
  Response $ 
  Enumerated $ 
  Right StatusInvalid