module EdgeNode.Controller.Http.SaveQualification (controller) where

import RetrofitProto
import KatipController
import ReliefJsonData
import Data.Aeson.Unit

controller :: KatipController (Alternative (Error Unit) SaveQualificationResponse)
controller = undefined