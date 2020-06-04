{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module EdgeNode.Statement.Mail (new, getMails, setAsProcessed) where

import EdgeNode.Mail

import qualified Hasql.Statement as HS
import Database.Transaction
import Control.Lens
import Data.String.Conv
import Hasql.TH
import Data.Aeson
import TH.Proto
import TH.Mk
import Data.Int
import qualified Data.Vector as V
import qualified Data.Text as T

mkArbitrary ''Type
mkArbitrary ''Status
mkArbitrary ''Value

instance ParamsShow Status where render = toS . fromStatus
instance ParamsShow Type where render = toS . fromType

new :: HS.Statement (T.Text, Type, Status, Value) ()
new =
  lmap (\x -> x & _2 %~ (toS . fromType) & _3 %~ (toS . fromStatus))
  [resultlessStatement|
    insert into edgenode.email
    (to_whom, type, status, value)
    values ($1 :: text, $2 :: text, $3 :: text, $4 :: jsonb)|]

getMails :: HS.Statement Status (V.Vector (Int64, Type, Value))
getMails =
  dimap (toS . fromStatus) (V.map (& _2 %~ (toType . toS))) $
  [vectorStatement|
    update edgenode.email
    set take_to_processing = now()
    where status = $1 :: text
    returning id :: int8, type :: text, value :: jsonb|]

setAsProcessed :: HS.Statement (Status, V.Vector Int64) ()
setAsProcessed =
  lmap (& _1 %~ (toS . fromStatus))
  [resultlessStatement|
    update edgenode.email
    set done = now(), status = $1 :: text
    where id = any($2 :: int8[])|]