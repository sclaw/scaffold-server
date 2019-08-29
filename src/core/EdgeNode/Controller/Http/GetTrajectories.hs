module EdgeNode.Controller.Http.GetTrajectories (controller) where

import EdgeNode.Model.User (UserId)

import RetrofitProto
import KatipController
import ReliefJsonData
import qualified Data.Text as T

controller :: UserId -> KatipController (Alternative (Error T.Text) GetTrajectoriesResponse)    
controller _ = undefined