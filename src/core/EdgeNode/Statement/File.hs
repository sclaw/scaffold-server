{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EdgeNode.Statement.File 
       ( save
       , getMeta
       , EdgeNode.Statement.File.delete
       , getHash
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Model.File

import qualified Hasql.Statement as HS
import Hasql.TH
import Control.Lens
import Control.Foldl
import Data.Coerce
import TH.Proto
import Data.List
import Control.Lens.Iso.Extended

save :: HS.Statement [(Hash, Name, Mime, EdgeNodeBucket)] [Id]
save =
  lmap (
      unzip4 
    . Prelude.map (\x -> 
      x & _1 %~ coerce 
        & _2 %~ coerce 
        & _3 %~ coerce 
        & _4 %~ (^.isoEdgeNodeBucket.stext))) $ 
  statement $ 
  premap (^.coerced) list
  where  
    statement = 
      [foldStatement|
        insert into storage.file 
        (hash, title, mime, bucket) 
        select x.hash, x.title, x.mime, x.bucket
        from unnest(
          $1 :: text[], 
          $2 :: text[], 
          $3 :: text[],
          $4 :: text[]) as x(hash, title, mime, bucket) 
        returning id :: int8|]

getMeta :: HS.Statement Id (Maybe (Hash, Name, Mime, Bucket))
getMeta = dimap (^.coerced) (fmap mkTpl) statement
  where 
    statement = 
      [maybeStatement| 
        select hash :: text, title :: text, mime :: text, bucket :: text 
        from storage.file
        where id = $1 :: int8 
              and not is_deleted|]
    mkTpl x = x & _1 %~ coerce & _2 %~ coerce & _3 %~ coerce & _4 %~ coerce 

delete :: HS.Statement Id ()
delete = lmap coerce statement
  where
    statement = 
      [singletonStatement|
        update storage.file 
        set deleted = now(), is_deleted = true 
        where id = $1 :: int8|]

getHash :: HS.Statement Id (Maybe Hash)
getHash = dimap (^.coerced) (fmap coerce) statement
  where 
    statement = 
      [maybeStatement|
        select hash :: text
        from storage.file
        where id = $1 :: int8|]