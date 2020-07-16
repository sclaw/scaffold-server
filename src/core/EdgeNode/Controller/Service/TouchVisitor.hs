{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Service.TouchVisitor (controller) where

import EdgeNode.Transport.Response

import KatipController
import Data.Aeson.Unit
import Hasql.TH
import Database.Transaction
import Control.Lens
import Network.IP.Addr
import Data.List
import Data.Text (pack)
import Data.Maybe

instance ParamsShow IP4 where render = intercalate "." . map show . ip4ToOctetList

controller :: Maybe IP4 -> KatipController (Response Unit)
controller ip = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (const (Ok Unit)) $
    katipTransaction hasql $
      statement
        (lmap (fromMaybe "unknown" . fmap (pack . intercalate "." . map show . ip4ToOctetList)) $
          [resultlessStatement|
            insert into public.visitor
            (day, ip, count)
            values (now() :: date, $1 :: text, 1)
            on conflict (day, ip) do
            update set count =
            (select count + 1
             from public.visitor
             where day = excluded.day and ip = excluded.ip)|]) ip