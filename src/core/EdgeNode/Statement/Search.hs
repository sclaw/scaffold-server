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
             piece, 
             word_similarity($1 :: text, piece) as sim
            from search.qualification
            where word_similarity($1 :: text, piece) >= 0.5
            limit 10),
          get_provider_matched_ids as 
           (select 
             piece, 
             word_similarity($1 :: text, piece) as sim
            from search.provider
            where word_similarity($1 :: text, piece) >= 0.5
            limit 10),
          get_combined_search as 
            (select piece, sim
             from get_qualification_matched_ids
             union 
             select piece, sim
             from get_provider_matched_ids
             order by sim desc
             limit 10)
        select piece :: text from get_combined_search|]

getQualificationList :: HS.Statement T.Text SearchQualificationList
getQualificationList = undefined