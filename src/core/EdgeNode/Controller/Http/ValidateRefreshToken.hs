{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Http.ValidateRefreshToken (controller) where

import EdgeNode.Api.Http.Auth.RefreshTokenValidator

import Proto
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

controller :: RefreshTokenValidatorRequest -> KatipController (Alternative (Error T.Text) RefreshTokenValidatorResponse)
controller req = do
  let token = req^._Wrapped'.field @"requestRefreshToken" 
  jw <- (^.katipEnv.jwk) `fmap` ask
  e <- liftIO $ runExceptT $ verifyToken (defaultJWTSettings jw) token
  case e of 
    Right _ -> 
      return $ 
      Fortune $ 
      RefreshTokenValidatorResponse $ 
      Response $ 
      Enumerated $ 
      Right StatusOk
    Left e -> 
      case e of 
        JWSError e -> 
          return $ 
          Error $ 
          ResponseError $ 
          show e^.stext
        JWTClaimsSetDecodeError e -> 
          return $ 
          Error $ 
          ResponseError $ 
          e^.stext
        _ -> 
          return $ 
          Fortune $ 
          RefreshTokenValidatorResponse $ 
          Response $ 
          Enumerated $ 
          Right StatusInvalid