module EdgeNode.Controller.Http.SaveQualification (controller) where

import RetrofitReqRespProto
import qualified Data.Text as T
import KatipController
import ReliefJsonData

controller :: KatipController (Alternative T.Text SaveQualificationResponse)
controller = undefined