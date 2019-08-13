module EdgeNode.Controller.Http.GetQualififcations (controller) where

import RetrofitProto
import KatipController
import ReliefJsonData
import qualified Data.Text as T

controller :: GetQualififcationsRequest -> KatipController (Alternative (Error T.Text) GetQualififcationsResponse)
controller _ = undefined