{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Qualification.AddQualificationToTrajectory (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.User
import EdgeNode.Statement.User as User

import Control.Lens.Iso.Extended
import Auth
import KatipController
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens
import Data.Vector.Lens
import Data.Word
import Data.Int
import Katip
import Data.Traversable
import Data.Functor

controller :: [WithId (Id "qualification") AddQualificationRequest] -> UserId -> KatipController (Response AddQualificationResponse)
controller qualifications user_id = do
  runTelegram (qualifications, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  resp <- katipTransactionViolationError hasql $
    statement User.addQualification (user_id, qualifications)
  $(logTM) DebugS (logStr (show resp))
  response <- fmap fromEither $ for resp $ \xs -> do
    let qualifications_ids = qualifications <&> \(WithField i _) -> i
    let add el tpl
          | el `elem` map fst xs = tpl & _1 %~ ((el^.coerced.integral @Int64 @Word64):)
          | otherwise = tpl & _2 %~ ((el^.coerced.integral @Int64 @Word64):)
    let mkResp (added, rejected) =
          AddQualificationResponse
          (added^.vector)
          (rejected^.vector)
    return $ (mkResp . foldr add ([], [])) qualifications_ids
  runTelegram response $> response