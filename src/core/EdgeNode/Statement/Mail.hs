{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module EdgeNode.Statement.Mail (new) where

import EdgeNode.Mail

import qualified Hasql.Statement as HS
import Database.Transaction
import Control.Lens
import Data.String.Conv
import Hasql.TH
import Data.Aeson
import TH.Proto

instance ParamsShow NewProviderAccount where
  render = show . toJSON

new :: HS.Statement NewProviderAccount ()
new =
  lmap ((toS (TypeNewProviderAccount^.isoType), (toS (StatusNew^.isoStatus)),) . toJSON)
  [resultlessStatement|
    insert into edgenode.email
    (type, status, value)
    values ($1 :: text, $2 :: text,  $3 :: jsonb)|]