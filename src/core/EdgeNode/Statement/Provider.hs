{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Statement.Provider (getBranches) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Provider

import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Aeson.WithField.Extended
import Control.Foldl
import Data.Coerce
import Control.Lens
import Data.Int
import qualified Data.Text as T ()

getBranches :: HS.Statement Id [(OptField "files" [Id] (OptField "image" Id Branch))]
getBranches = lmap (coerce @_ @Int64) $ statement $ premap mkBranch list
  where
    statement = undefined
    mkBranch = undefined