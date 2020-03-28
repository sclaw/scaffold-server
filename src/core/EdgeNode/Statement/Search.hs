{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Statement.Search (getBarItems) where

import EdgeNode.Transport.Search

import qualified Hasql.Statement as HS
import Hasql.TH
import qualified Data.Text as T
import Control.Lens.Iso.Extended
import Control.Lens
import Control.Foldl

getBarItems :: HS.Statement T.Text SearchBar
getBarItems = statement $ (fmap SearchBar . premap (^.from lazytext)) vector
  where
    statement = 
      [foldStatement|
        with get_qualification_matched_ids as
         (select x.ident
          from (
           select 
            distinct provider_branch_qualification_fk as ident, 
            similarity($1 :: text, piece) as match_per 
           from search.qualification) as x
          where (x.match_per > 0)
          order by x.match_per desc
          limit 10) 
        select title :: text
        from edgenode.provider_branch_qualification 
        where id = any(select ident from get_qualification_matched_ids)|]