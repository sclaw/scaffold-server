{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Statement.Search
       ( getBarItems
       , getQualificationList
       ) where

import EdgeNode.Transport.Search
import EdgeNode.Transport.Iso
import EdgeNode.Statement.Provider ()

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
             distinct piece, 
             word_similarity($1 :: text, piece) as sim
            from search.qualification
            where word_similarity($1 :: text, piece) >= 0.5
            limit 10),
          get_provider_matched_ids as 
           (select 
             distinct piece, 
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
            order by sim desc)
        select distinct piece :: text from get_combined_search limit 10|]

getQualificationList :: HS.Statement T.Text SearchQualificationList
getQualificationList = statement $ (fmap SearchQualificationList . premap mkItem) vector
  where
    mkItem x = 
      SearchQualificationItem
      (x^._1.integral) 
      (x^._2.from lazytext) 
      (x^._3.from qualQualificationDegree) 
      (x^._4.from lazytext)
    statement =
      [foldStatement|
        with 
          get_branch_qualifications as 
           (select pbq.id, pbq.title, pbq.type as degree_type, pb.title as branch_title
            from search.qualification as sq 
            inner join edgenode.provider_branch_qualification as pbq
            on sq.provider_branch_qualification_fk = pbq.id
            inner join edgenode.provider_branch as pb 
            on pbq.provider_branch_fk = pb.id
            where word_similarity($1 :: text, sq.piece) >= 0.5 and not pbq.is_deleted),
          get_ordinary_qualifications as
           (select pbq.id, pbq.title, pbq.type as degree_type, pb.title as branch_title
            from search.provider as sp 
            inner join edgenode.provider_branch as pb
            on sp.provider_branch_fk = pb.id
            left join edgenode.provider_branch_qualification as pbq
            on pb.id = pbq.provider_branch_fk
            where word_similarity($1 :: text, sp.piece) >= 0.5 and pbq.id is not null and not pbq.is_deleted),
          get_combined_search as 
           (select * from get_branch_qualifications 
            union 
            select * from get_ordinary_qualifications)
        select distinct id :: int8, title :: text, 
        degree_type :: text, branch_title :: text 
        from get_combined_search order by branch_title, title|]