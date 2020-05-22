{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Statement.Statistics
       ( registrations
       , activeUsers
       , apiCaller
       , apiCounter
       ) where

import EdgeNode.Transport.Id

import Auth
import qualified Hasql.Statement as HS
import Hasql.TH
import Control.Lens
import Control.Foldl
import qualified Data.Text as T
import Data.Int
import Data.String.Conv
import Data.Coerce

registrations :: HS.Statement (Maybe Int32) [(T.Text, Int32)]
registrations =
  [foldStatement|
     with start_day as (
       select created as day
       from auth.user
       order by created
       asc limit 1)
     select d.day :: date, count(u.*) :: int4
     from (select t.day :: date
       from generate_series(
         (select
           coalesce(now() - cast($1 :: int4? || ' days' as interval), day)
          from start_day),
         'today',
         interval '1 day') as t(day)) as d
     left join auth.user as u
     on d.day = u.created :: date
     and u.user_type = 'primary'
     group by d.day
     order by d.day|] $
  premap (& _1 %~ (toS . show)) list

activeUsers :: HS.Statement (Maybe Int32) [(T.Text, Int32)]
activeUsers =
  [foldStatement|
     with start_day as (
       select created as day
       from auth.user
       order by created
       asc limit 1)
     select d.day :: date, count(distinct t.user_fk) :: int4
     from (select t.day :: date
       from generate_series(
         (select
           coalesce(now() - cast($1 :: int4? || ' days' as interval), day)
          from start_day),
         'today',
         interval '1 day') as t(day)) as d
     left join auth.token as t
     on d.day = t.created :: date
     group by d.day
     order by d.day|] $
  premap (& _1 %~ (toS . show)) list

apiCaller :: HS.Statement (T.Text, UserId) ()
apiCaller =
  lmap (& _2 %~ (coerce @UserId @Int64)) $
  [resultlessStatement|
    insert into stat.api_call_counter (api, created, user_fk)
    values ($1 :: text, now(), $2 :: int8)|]

apiCounter :: HS.Statement (Maybe Int32) [(T.Text, Int32)]
apiCounter = undefined