{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Statement.File (save) where

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