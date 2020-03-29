{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Statement.Search
       ( getBarItems
       , getQualificationList
       ) where

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
        with 
          get_qualification_matched_ids as 
           (select 
             provider_branch_qualification_fk as ident, 
             origin, 
             word_similarity($1 :: text, piece) as sim
            from search.qualification
            where word_similarity($1 :: text, piece) >= 0.5
            order by ident
            limit 10),
          get_provider_matched_ids as 
           (select 
             provider_branch_fk as ident, 
             origin, 
             word_similarity($1 :: text, piece) as sim
            from search.provider
            where word_similarity($1 :: text, piece) >= 0.5
            order by ident
            limit 10),
          get_combined_search as 
            (select pbq.title, sq.sim
             from get_qualification_matched_ids as sq 
             inner join edgenode.provider_branch_qualification as pbq
             on sq.ident = pbq.id
             union 
             select pb.title, sb.sim
             from get_provider_matched_ids as sb 
             inner join edgenode.provider_branch as pb
             on sb.ident = pb.id
             order by sim desc
             limit 10)
        select title :: text from get_combined_search|]

getQualificationList :: HS.Statement T.Text SearchQualificationList
getQualificationList = undefined