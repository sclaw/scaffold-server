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

newtype FeedbackApi route =
        FeedbackApi
        { _feedbackApiPutFeedback
          :: route
          :- Description "put feedback"
          :> ReqBody '[JSON] Feedback
          :> Put '[JSON] (Response Unit)
        } deriving stock Generic