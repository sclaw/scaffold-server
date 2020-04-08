module EdgeNode.Controller.User.Qualification.GetCountriesByDegreeType (controller) where

import EdgeNode.Transport.Response

import TH.Proto
import KatipController

controller :: EdgeNodeProviderCategory -> EdgeNodeQualificationDegree -> KatipController (Response [EdgeNodeCountry])
controller _ _ = pure $ Ok []