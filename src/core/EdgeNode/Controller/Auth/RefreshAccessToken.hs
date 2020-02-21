{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Controller.Auth.RefreshAccessToken (controller) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response hiding (Error)
import qualified EdgeNode.Transport.Error as Error
import qualified EdgeNode.Statement.Auth as Auth
import EdgeNode.Transport.Id
import EdgeNode.Controller.Auth.SignIn (mkTokens)

import TH.Proto
import Katip
import Auth
import KatipController
import Data.Generics.Product.Fields
import Control.Lens
import Control.Monad.Except
import Servant.Auth.Server
import qualified Data.Text as T
import Data.Bifunctor
import Data.Traversable
import qualified Crypto.JWT as JWT
import Database.Transaction
import Data.Aeson
import Data.Generics.Product.Positions
import Control.Lens.Iso.Extended

controller :: Token -> Id "user" -> KatipController (Response Tokens)
controller token user_id = do
  key <- fmap (^.katipEnv.jwk) ask 
  verify_result <- 
    fmap (first mkJWTError) $ 
    liftIO $ 
    runExceptT $ 
    verifyToken
    (defaultJWTSettings key) 
    (token^.field @"tokenToken")
  fmap (fromEither . first (Error.asError @T.Text) . join) $ 
    for verify_result $ \claims -> do 
      hasql <- fmap (^.katipEnv.hasqlDbPool) ask
      let uqm = claims^.JWT.unregisteredClaims.at "dat".to (fmap fromJSON)
      case uqm of 
        Just (Success uq) -> do
          user_role_m <- katipTransaction hasql $ 
            statement Auth.checkRefreshToken (uq, user_id)
          case user_role_m of
            Just user_role -> 
              fmap (second (^.position @2.position @2)) $ 
              mkTokens (user_id, user_role)
            Nothing -> pure $ Left "token not found"
        Just (Error e) -> do 
          $(logTM) ErrorS (logStr ("dat json decoding error: " <> e))  
          pure $ Left "dat json decoding error"  
        Nothing -> pure $ Left "dat not found"

mkJWTError :: JWT.JWTError -> T.Text
mkJWTError JWT.JWTExpired = ErrorRefreshTokenExpired^.isoError.stext
mkJWTError _ = ErrorJWTError^.isoError.stext