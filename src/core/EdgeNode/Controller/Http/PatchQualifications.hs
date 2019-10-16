module EdgeNode.Controller.Http.PatchQualifications (controller) where

import EdgeNode.Model.User

import Proto
import Json
import KatipController
import Data.Aeson.Unit
import qualified Data.Text as T

controller :: PatchQualificationsRequest -> UserId -> KatipController (Alternative (Error T.Text) Unit)
controller _ _ = undefined