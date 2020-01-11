{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Statement.File (save) where

import EdgeNode.Transport.Id

import qualified Hasql.Statement as HS
import qualified Data.Text as T
import Data.Coerce
import Hasql.TH
import Control.Lens
import Data.Int

save :: HS.Statement (T.Text, T.Text, T.Text) Id
save = rmap (coerce @Int64 @Id) statement
  where  
    statement = 
      [singletonStatement|
        insert into storage.file 
        (title, mime, hash) 
        values ($1 :: text, $2 :: text, $3 :: text) 
        returning id :: int8|]