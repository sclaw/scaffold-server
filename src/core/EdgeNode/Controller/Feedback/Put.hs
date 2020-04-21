{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Feedback.Put (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Feedback
import qualified EdgeNode.Transport.Validator as Validator
import EdgeNode.Statement.Feedback as Feedback
import qualified EdgeNode.Transport.Error as Error

import KatipController
import Data.Aeson.Unit
import Database.Transaction
import Control.Lens
import Data.Traversable
import Katip
import Pretty
import Data.Aeson.WithField
import qualified Data.Text as T
import Data.Validation
import Data.Bifunctor
import qualified Network.HTTP.Client as HTTP
import Control.Monad.IO.Class
import Data.Aeson
import Data.Time.Clock
import Data.String.Conv

controller :: WithField "recaptcha" T.Text Feedback -> KatipController (Response Unit)
controller (WithField recaptcha_resp feedback) = do
  $(logTM) DebugS (logStr (mkPretty mempty feedback))
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  captcha <-
    fmap ( first (map asError)
         . Data.Validation.fromEither)
    (verifyReCaptcha recaptcha_resp)
  fmap (fromValidation . bimap (map asError) (const Unit)) $
    for (captcha *> first (map asError) (Validator.feedback feedback)) $
      const $ katipTransaction hasql $ statement Feedback.put feedback

data ReCaptchaError =
       ReCaptchaErrorKeyNF
     | ReCaptchaErrorKeyInvalid (Maybe [T.Text])
     | ReCaptchaErrorJSONFail String
     deriving stock Show

instance Error.AsError ReCaptchaError where
  asError ReCaptchaErrorKeyNF = asError @T.Text "recaptcha secret key not found"
  asError (ReCaptchaErrorKeyInvalid es) = asError @T.Text $ "recaptcha not valid: " <> maybe mempty (T.intercalate ", ") es
  asError (ReCaptchaErrorJSONFail e) = asError @T.Text $ "repatcha json error " <> T.pack e

data ReCaptchaResp =
     ReCaptchaResp
     { reCaptchaRespSuccess :: Bool
     , reCaptchaRespTm :: Maybe UTCTime  -- timestamp of the challenge load (ISO format yyyy-MM-dd'T'HH:mm:ssZZ)
     , reCaptchaResHostname :: Maybe T.Text -- the hostname of the site where the reCAPTCHA was solved
     , reCaptchaRespErrorCodes :: Maybe [T.Text] -- optional
     } deriving stock Show

instance FromJSON ReCaptchaResp where
  parseJSON = withObject "ReCaptchaResp" $ \o -> do
    reCaptchaRespSuccess <- o .: "success"
    reCaptchaRespTm <- o .:? "challenge_ts"
    reCaptchaResHostname <- o .:? "hostname"
    reCaptchaRespErrorCodes <- o .:? "error-codes"
    pure ReCaptchaResp {..}

verifyReCaptcha :: T.Text -> KatipController (Either [ReCaptchaError] ())
verifyReCaptcha recaptcha_resp = do
  mgr <- fmap (^.katipEnv.httpReqManager) ask
  keys <- fmap (^.katipEnv.apiKeys) ask
  case lookup "recaptcha" keys of
    Nothing -> pure $ Left [ReCaptchaErrorKeyNF]
    Just key -> do
      req <- HTTP.parseRequest "https://www.google.com/recaptcha/api/siteverify"
      resp <- liftIO $ flip HTTP.httpLbs mgr $
        HTTP.setQueryString
        [ ("secret", Just (toS key))
        , ("response", Just (toS recaptcha_resp))
        ] req { HTTP.method = "POST" }
      $(logTM) DebugS (logStr (mkPretty mempty resp))
      let recaptcha_resp_e = eitherDecode @ReCaptchaResp (HTTP.responseBody resp)
      case recaptcha_resp_e of
        Left e -> pure $ Left [ReCaptchaErrorJSONFail e]
        Right x -> pure $
          if reCaptchaRespSuccess x
          then Right ()
          else Left [ReCaptchaErrorKeyInvalid (reCaptchaRespErrorCodes x)]