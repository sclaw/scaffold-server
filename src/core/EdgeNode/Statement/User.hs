{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Statement.User (new) where

import EdgeNode.Model.User
import EdgeNode.Transport.Id

import qualified Hasql.Statement as HS
import Control.Lens
import Hasql.TH

new :: HS.Statement User (Id "user")
new = dimap encoder (^.coerced) statement
  where
    statement = 
      [singletonStatement|
        with 
          getFullDay as (
           insert into public.full_day 
           (year, month, day) 
           values ($4 :: int4, $5 :: int4, $6 :: int4) 
           returning id),
          getUser as (
           insert into auth.user
           (identifier, password, user_type)
           values ('', '', '')
           returning id)
        insert into edgenode.user 
        (name, middlename, surname, birthday_id, 
         allegiance, avatar_id, gender, status, user_id) 
        values ($1 :: text, $2 :: text, $3 :: text, (select id from getFullDay), 
                $7 :: text, $8 :: int8?, $9 :: int4, '', (select id from getUser))
        returning id :: int8|]