{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Service.TouchVisitor (controller) where

import EdgeNode.Transport.Response

import KatipController
import Data.Aeson.Unit
import Hasql.TH
import Database.Transaction
import Control.Lens

controller :: KatipController (Response Unit)
controller = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (const (Ok Unit)) $
    katipTransaction hasql $
      statement
        [resultlessStatement|
          insert into public.visitor
          (day, count)
          values (now() :: date, 1)
          on conflict (day) do
          update set count =
          (select count + 1
           from public.visitor
           where day = excluded.day)|] ()