module EdgeNode.Statement.Search (getQualification) where

import EdgeNode.Transport.Search

import qualified Hasql.Statement as HS
import qualified Data.Text as T

getQualification :: HS.Statement T.Text [Qualification]
getQualification = undefined
