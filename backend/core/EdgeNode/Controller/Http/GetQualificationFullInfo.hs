module EdgeNode.Controller.Http.GetQualificationFullInfo (controller) where

import RetrofitProto
import KatipController
import ReliefJsonData
import Data.Aeson.Unit

controller :: KatipController (Alternative (Error Unit) GetQualificationFullInfoResponse)
controller = undefined