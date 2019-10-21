module EdgeNode.Controller.Http.DeleteQualification (controller) where

import EdgeNode.Model.User
import EdgeNode.User.Qualification

import Json
import KatipController
import Data.Aeson.Unit
import qualified Data.Text as T

controller :: UserQualificationId -> UserId -> KatipController (Alternative (Error T.Text) Unit)
controller _ _ = return $ Fortune Unit