module EdgeNode.Controller.Http.SearchQualification (controller) where

import EdgeNode.Search
import EdgeNode.Provider.Qualification

import ReliefJsonData
import KatipController
import qualified Data.Text as T

controller :: SearchPiece -> Maybe T.Text -> KatipController (Alternative (Error T.Text) XQualificationFullInfo)
controller _ _ = undefined