{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}


module EdgeNode.Statement.Statistics
       ( registrations
       , activeUsers
       , apiCaller
       , apiCounter
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Statistics

import Auth
import qualified Hasql.Statement as HS
import Hasql.TH
import Control.Lens
import Control.Foldl
import qualified Data.Text as T
import Data.Int
import Data.String.Conv
import Data.Coerce
import Data.Aeson
import qualified Data.Vector as V

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
    insert into stat.api_call_counter (api, user_fk)
    values ($1 :: text, $2 :: int8)|]

apiCounter :: HS.Statement (Maybe Int32) (Either T.Text [ApiCounter_Value])
apiCounter =
  [foldStatement|
     with start_day as (
       select created as day
       from auth.user
       order by created
       asc limit 1),
     api_counter as (
       select created, api, count(distinct "user_fk") as cnt
       from stat.api_call_counter
       group by created, api)
     select
      d.day :: date,
      array_agg(jsonb_build_object(
        'api',
        ac.api,
        'count',
        ac.cnt) order by api) filter (where api is not null) :: jsonb[]?
     from (select t.day :: date
       from generate_series(
         (select
           coalesce(now() - cast($1 :: int4? || ' days' as interval), day)
          from start_day),
         'today',
         interval '1 day') as t(day)) as d
     left join api_counter as ac
     on d.day = ac.created
     group by d.day
     order by d.day|] $
  fmap (resultToEither . sequence) $ premap (uncurry mk) list
  where
    mk date (Just (v :: V.Vector Value)) =
      fmap (ApiCounter_Value ((toS . show) date))
      (sequence (V.map (fromJSON @ApiCounter_Api) v))
    mk date Nothing = Success $ ApiCounter_Value ((toS . show) date) mempty
    resultToEither (Success x) = Right x
    resultToEither (Error e) = Left $ toS e