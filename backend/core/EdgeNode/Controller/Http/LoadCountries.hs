module EdgeNode.Controller.Http.LoadCountries (controller) where

import EdgeNode.Service.Data (ServiceLanguage)

import ReliefJsonData
import KatipController
import RetrofitProto
import qualified Data.Text as T

controller :: ServiceLanguage -> KatipController (Alternative T.Text CountriesResponse)
controller _ = undefined