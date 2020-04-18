{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Feedback.Put (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Feedback
import qualified EdgeNode.Transport.Validator as Validator
import EdgeNode.Statement.Feedback as Feedback

import KatipController
import Data.Aeson.Unit
import Database.Transaction
import Control.Lens
import Data.Traversable
import Katip
import Pretty

controller :: Feedback -> KatipController (Response Unit)
controller feedback = do
  $(logTM) DebugS (logStr (mkPretty mempty feedback))
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (fromValidation . bimap (map asError) (const Unit)) $
    for (Validator.feedback feedback) $ const $
      katipTransaction hasql $ statement Feedback.put feedback