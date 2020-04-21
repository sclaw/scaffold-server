{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Feedback (FeedbackApi (..)) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Feedback

import Servant.API.Generic
import Servant.API
import Data.Aeson.Unit
import qualified Data.Text as T
import Data.Aeson.WithField

newtype FeedbackApi route =
        FeedbackApi
        { _feedbackApiPutFeedback
          :: route
          :- Description "put feedback"
          :> ReqBody '[JSON] (WithField "recaptcha" T.Text Feedback)
          :> Put '[JSON] (Response Unit)
        } deriving stock Generic