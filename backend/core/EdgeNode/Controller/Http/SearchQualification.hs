module EdgeNode.Controller.Http.SearchQualification (controller) where

import EdgeNode.Search
import Database.FullText.Search ()

import RetrofitProto
import ReliefJsonData
import KatipController
import qualified Data.Text as T

controller :: SearchPiece -> QualificationRequest -> KatipController (Alternative (Error T.Text) QualificationResponse)
controller _ _ = undefined