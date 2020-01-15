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

save :: HS.Statement [(Hash, Name, Mime)] [Id]
save =
  lmap (unzip3 . Prelude.map (\x -> x & _1 %~ coerce & _2 %~ coerce & _3 %~ coerce)) $ 
  statement $ 
  premap (^.coerced) list
  where  
    statement = 
      [foldStatement|
        insert into storage.file 
        (hash, title, mime) 
        select x.hash, x.title, x.mime
        from unnest(
          $1 :: text[], 
          $2 :: text[], 
          $3 :: text[]) as x(hash, title, mime) 
        returning id :: int8|]