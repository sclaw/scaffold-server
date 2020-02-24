{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Provider.PatchBranch (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Id
import qualified EdgeNode.Statement.Provider as Provider

import Katip
import KatipController
import Data.Aeson.WithField.Extended
import Data.Aeson.Unit
import Pretty
import Data.Generics.Product.Positions
import Control.Lens
import Database.Transaction

controller :: [WithId (Id "branch") (OptField "files" [Id "file"] (OptField "image" (Id "img") Branch))] -> Id "user" -> KatipController (Response Unit)
controller xs _ = do
  $(logTM) DebugS (logStr ("branches to be patched: " ++ mkPretty mempty xs))
  let xs' = xs <&> \x -> 
        ( x^.position @1
        , x^.position @2.position @1.position @2.position @1.position @1
        , x^.position @2.position @1.position @2.position @1.position @2)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (const (Ok Unit)) $ 
    katipTransactionViolationError hasql $ 
    statement Provider.updateBranches xs' 