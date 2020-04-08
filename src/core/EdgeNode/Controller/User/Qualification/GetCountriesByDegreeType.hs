module EdgeNode.Controller.User.Qualification.GetCountriesByDegreeType (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries (EdgeNodeCountryCapture)

import TH.Proto
import KatipController

controller :: EdgeNodeProviderCategory -> EdgeNodeQualificationDegree -> KatipController (Response [EdgeNodeCountryCapture])
controller _ _ = pure $ Ok []