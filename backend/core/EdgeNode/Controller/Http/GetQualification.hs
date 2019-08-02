module EdgeNode.Controller.Http.GetQualification (controller) where

import RetrofitReqRespProto
import qualified Data.Text as T
import KatipController
import ReliefJsonData

controller :: KatipController (Alternative T.Text GetQualificationResponse)
controller = undefined