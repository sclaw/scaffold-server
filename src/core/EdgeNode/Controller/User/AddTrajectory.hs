{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.User.AddTrajectory (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User
import qualified EdgeNode.Transport.Error as Error

import Auth
import KatipController
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens
import Data.Aeson.Unit
import qualified Data.Text as T

controller :: OnlyField "id" (Id "qualification") -> UserId -> KatipController (Response Unit)
controller qualification_id user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let mkError _ = Error.asError @T.Text "qualification is already at your trajectory's list"
  fmap (fromEither . bimap mkError (const Unit)) $
    katipTransactionViolationError hasql $
      statement
      User.addTrajectory
      (user_id, qualification_id)