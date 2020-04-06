{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Statement.Search
       ( getBarItems
       , getQualificationList
       , getQualificationModal
       , getAuthorizedQualificationList
       ) where

import EdgeNode.Transport.Search
import EdgeNode.Transport.Iso
import EdgeNode.Statement.Provider ()
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Id

import Auth
import TH.Mk
import TH.Proto
import qualified Hasql.Statement as HS
import Hasql.TH
import qualified Data.Text as T
import Control.Lens.Iso.Extended
import Control.Lens
import Control.Foldl
import Database.Transaction
import Data.Aeson.WithField
import Data.Coerce
import Data.Int

mkArbitrary ''ProviderCategory

getBarItems :: HS.Statement T.Text SearchBar
getBarItems = statement $ (fmap SearchBar . premap (^.from lazytext)) vector
  where
    statement =
      [foldStatement|
        with
          get_qualification_matched_ids as
           (select
             distinct sq.piece,
             word_similarity($1 :: text, sq.piece) as sim
            from search.qualification as sq
            inner join edgenode.provider_branch_qualification as pbq
            on sq.provider_branch_qualification_fk = pbq.id
            inner join edgenode.provider_branch_public as pbp
            on pbq.provider_branch_fk = pbp.provider_branch_fk
            where word_similarity($1 :: text, sq.piece) >= 0.5 and sq.is_public
            limit 10),
          get_provider_matched_ids as
           (select
             distinct sp.piece,
             word_similarity($1 :: text, sp.piece) as sim
            from search.provider as sp
            inner join edgenode.provider_branch_public as pbp
            on sp.provider_branch_fk = pbp.provider_branch_fk
            where word_similarity($1 :: text, sp.piece) >= 0.5 and sp.is_public
            limit 10),
          get_combined_search as
           (select piece, sim
            from get_qualification_matched_ids
            union
            select piece, sim
            from get_provider_matched_ids
            order by sim desc)
        select distinct piece :: text from get_combined_search limit 10|]

instance ParamsShow ProviderCategory where
  render = (^.isoProviderCategory)

getQualificationList :: HS.Statement (T.Text, ProviderCategory) SearchQualificationList
getQualificationList = lmap mkEncoder $ statement $ (fmap SearchQualificationList . premap mkItem) vector
  where
    mkEncoder x = x & _2 %~ (^.isoProviderCategory.stext)
    mkItem x =
      SearchQualificationItem
      (x^._1.integral)
      (x^._2.from lazytext)
      (x^._3.from qualQualificationDegree)
      (x^._4.from lazytext)
      False
    statement =
      [foldStatement|
        with
          get_branch_qualifications as
           (select pbq.id, pbq.title, pbq.type as degree_type, pbp.title as branch_title
            from search.qualification as sq
            inner join edgenode.provider_branch_qualification as pbq
            on sq.provider_branch_qualification_fk = pbq.id
            inner join edgenode.provider_branch as pb
            on pbq.provider_branch_fk = pb.id
            inner join edgenode.provider_branch_public as pbp
            on pbp.provider_branch_fk = pb.id
            inner join edgenode.provider as p
            on p.id = pb.provider_fk
            where word_similarity($1 :: text, sq.piece) >= 0.5
            and not pbq.is_deleted and p.category = $2 :: text and sq.is_public),
          get_ordinary_qualifications as
           (select pbq.id, pbq.title, pbq.type as degree_type, pbp.title as branch_title
            from search.provider as sp
            inner join edgenode.provider_branch as pb
            on sp.provider_branch_fk = pb.id
            inner join edgenode.provider_branch_public as pbp
            on pbp.provider_branch_fk = pb.id
            inner join edgenode.provider as p
            on p.id = pb.provider_fk
            left join edgenode.provider_branch_qualification as pbq
            on pbq.provider_branch_fk = pb.id
            where word_similarity($1 :: text, sp.piece) >= 0.5
            and pbq.id is not null and not pbq.is_deleted and p.category = $2 :: text and sp.is_public),
          get_combined_search as
           (select * from get_branch_qualifications
            union
            select * from get_ordinary_qualifications)
        select distinct id :: int8, title :: text,
        degree_type :: text, branch_title :: text
        from get_combined_search order by branch_title, title|]

getAuthorizedQualificationList :: HS.Statement (T.Text, ProviderCategory, UserId) SearchQualificationList
getAuthorizedQualificationList = lmap mkEncoder $ statement $ (fmap SearchQualificationList . premap mkItem) vector
  where
    mkEncoder x = x & _2 %~ (^.isoProviderCategory.stext) & _3 %~ coerce @_ @Int64
    mkItem x =
      SearchQualificationItem
      (x^._1.integral)
      (x^._2.from lazytext)
      (x^._3.from qualQualificationDegree)
      (x^._4.from lazytext)
      (x^._5)
    statement =
      [foldStatement|
        with
          get_branch_qualifications as
           (select pbq.id, pbq.title, pbq.type as degree_type, pbp.title as branch_title
            from search.qualification as sq
            inner join edgenode.provider_branch_qualification as pbq
            on sq.provider_branch_qualification_fk = pbq.id
            inner join edgenode.provider_branch as pb
            on pbq.provider_branch_fk = pb.id
            inner join edgenode.provider_branch_public as pbp
            on pbp.provider_branch_fk = pb.id
            inner join edgenode.provider as p
            on p.id = pb.provider_fk
            where word_similarity($1 :: text, sq.piece) >= 0.5
            and not pbq.is_deleted and p.category = $2 :: text and sq.is_public),
          get_ordinary_qualifications as
           (select pbq.id, pbq.title, pbq.type as degree_type, pbp.title as branch_title
            from search.provider as sp
            inner join edgenode.provider_branch as pb
            on sp.provider_branch_fk = pb.id
            inner join edgenode.provider_branch_public as pbp
            on pbp.provider_branch_fk = pb.id
            inner join edgenode.provider as p
            on p.id = pb.provider_fk
            left join edgenode.provider_branch_qualification as pbq
            on pbq.provider_branch_fk = pb.id
            where word_similarity($1 :: text, sp.piece) >= 0.5
            and pbq.id is not null and not pbq.is_deleted and p.category = $2 :: text and sp.is_public),
          get_combined_search as
           (select * from get_branch_qualifications
            union
            select * from get_ordinary_qualifications)
        select distinct c.id :: int8, c.title :: text,
        c.degree_type :: text, c.branch_title :: text,
        (ut.id is not null) :: bool
        from get_combined_search as c
        left join edgenode.user_trajectory as ut
        on c.id = ut.provider_branch_qualification_fk and ut.user_fk = $3 :: int8
        order by branch_title, title|]

getQualificationModal :: HS.Statement (Id "qualification" ) (Maybe (WithField "image" (Maybe (Id "file")) SearchQualificationModal))
getQualificationModal = dimap coerce (fmap mkEncoder) statement
  where
    mkEncoder x =
      WithField (x^?_1._Just.coerced) $
      SearchQualificationModal
      (x^._2.from lazytext)
      (x^._3.from country)
      (x^._4.from lazytext)
      (x^._5.from qualStudyTime)
      Nothing
      0
    statement =
      [maybeStatement|
        select
          pbp.image_fk :: int8?,
          pbq.title :: text,
          pbp.country :: text,
          pbp.title :: text,
          pbq.study_time :: text
        from edgenode.provider_branch_qualification as pbq
        inner join edgenode.provider_branch_public as pbp
        on pbq.provider_branch_fk = pbp.provider_branch_fk
        where pbq.id = $1 :: int8|]