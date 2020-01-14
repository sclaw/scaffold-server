{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Statement.File (save) where

import EdgeNode.Transport.Id

import qualified Hasql.Statement as HS
import qualified Data.Text as T
import Hasql.TH
import Control.Lens
import Control.Foldl

save :: HS.Statement [(T.Text, T.Text, T.Text)] [Id]
save = lmap unzip3 $ statement $ premap (^.coerced) list
  where  
    statement = 
      [foldStatement|
        insert into storage.file 
        (title, mime, hash) 
        select t, m, h
        from unnest(
          $1 :: text[], 
          $2 :: text[], 
          $3 :: text[]) as x(t, m, h) 
        returning id :: int8|]