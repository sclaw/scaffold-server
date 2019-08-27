module EdgeNode.Controller.Http.GetTrajectories (controller) where

import RetrofitProto
import KatipController
import ReliefJsonData
import qualified Data.Text as T

controller :: KatipController (Alternative (Error T.Text) GetTrajectoriesResponse)    
controller = undefined