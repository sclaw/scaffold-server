{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Controller.Auth.SignIn (controller, mkTokens) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.Auth
import qualified EdgeNode.Statement.Auth as Auth
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Model.User

import Katip
import KatipController
import Control.Lens
import Data.Derive.Default
import Data.DeriveTH
import Data.Default
import Default ()
import Database.Transaction
import Data.Traversable
import Data.Maybe
import qualified Data.Text as T
import Data.Password
import Data.Generics.Product.Fields
import Control.Lens.Iso.Extended
import Crypto.JOSE.Compact as Jose
import System.Random.PCG.Unique
import Hash
import Control.Monad.IO.Class
import qualified Auth as Auth
import Control.Monad.Except
import Data.Bifunctor
import Data.Aeson.WithField
import Data.Tuple.Ops

derive makeDefault ''Tokens

controller :: SigninReq -> KatipController (Response (WithId (Id "user") (WithField "role" UserRole Tokens)))
controller req = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  cred <- katipTransaction hasql $ statement Auth.getUserCred req
  fmap (fromMaybe (Error (Error.asError @T.Text "credential not found"))) $ 
    for cred $ \x -> do 
      let check = checkPass (req^.field @"signinReqPassword".lazytext.to mkPass) (x^._3)
      case check of 
        PassCheckSuccess -> fmap (fromEither . first (Error.asError @T.Text)) $ mkTokens (initT x)
        PassCheckFail -> pure $ Error (Error.asError @T.Text "wrong password")

mkTokens :: (Id "user", UserRole) -> KatipController (Either T.Text (WithId (Id "user") (WithField "role" UserRole Tokens)))
mkTokens cred = do 
  key <- fmap (^.katipEnv.jwk) ask
  unique <- liftIO $ fmap mkHash (uniformW64 =<< createSystemRandom)
  access <- liftIO $ runExceptT (Auth.mkAccessToken key (cred^._1) unique (cred^._2))
  refresh <- liftIO $ runExceptT (Auth.mkRefreshToken key (cred^._1))
  let encode x = x^.to Jose.encodeCompact.bytesLazy
  $(logTM) DebugS (logStr ("access token: " <> show (second (first encode) access))) 
  $(logTM) DebugS (logStr ("refresh token: " <> show (second encode refresh)))
  x <- for ((,) <$> access <*> refresh) $
    \(a, r) -> do
      hasql <- fmap (^.katipEnv.hasqlDbPool) ask
      void $ katipTransaction hasql $ statement Auth.putRefreshToken (encode r, cred^._1, unique)
      pure $ WithField (cred^._1) $ WithField (cred^._2) $
        def & field @"tokensAccessToken" .~ encode (a^._1)
            & field @"tokensRefreshToken" .~ encode r
            & field @"tokensLifetime" ?~ (a^._2)
  case x of Right resp -> pure $ Right resp; Left e -> pure $ Left (show e^.stext)           