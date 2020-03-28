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
          select provider_branch_qualification_fk as ident 
          from search.qualification
          where word_similarity($1 :: text, piece) >= 0.5 
          limit 10) as x) 
        select title :: text
        from edgenode.provider_branch_qualification 
        where id = any(select ident from get_qualification_matched_ids) 
        order by title asc|]