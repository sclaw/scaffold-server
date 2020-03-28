{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Statement.Search (getQualification) where

import EdgeNode.Transport.Search

import qualified Hasql.Statement as HS
import Hasql.TH
import qualified Data.Text as T
import Control.Lens.Iso.Extended
import Control.Lens
import Control.Foldl

getQualification :: HS.Statement T.Text [SearchQualification]
getQualification = statement $ premap mkItem list
  where
    statement = 
      [foldStatement|
        select id :: int8, title :: text
        from edgenode.provider_branch_qualification 
        where id = any(select id from search.qualification where $1 :: text % piece)|]
    mkItem x = SearchQualification (x^._1.integral) (x^._2.from lazytext)
