module EdgeNode.Controller.Http.SearchInit (controller) where

import EdgeNode.Provider.Qualification

import ReliefJsonData
import KatipController
import qualified Data.Text as T

controller :: KatipController (Alternative (Error T.Text) XQualificationFullInfo)
controller = undefined