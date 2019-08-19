module EdgeNode.Controller.Http.SearchQualification (controller) where

import EdgeNode.Model.Provider

import RetrofitProto
import ReliefJsonData
import KatipController
import qualified Data.Text as T

controller :: SearchPiece -> SearchQualificationRequest -> KatipController (Alternative (Error T.Text) SearchQualificationResponse)
controller _ _ = undefined

