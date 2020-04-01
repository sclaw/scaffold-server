{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Search.GetQualificationModal (controller) where

import EdgeNode.Transport.Response
-- import EdgeNode.Transport.Search
import qualified EdgeNode.Statement.Search as Search ()
import EdgeNode.Transport.Search
import EdgeNode.Transport.Id

-- import Katip
import KatipController
-- import qualified Data.Text as T
import Data.Aeson.WithField.Extended
-- import Data.Maybe
-- import Data.Traversable
-- import Data.Coerce
-- import Database.Transaction
-- import Control.Lens

controller :: Id "qualification" -> KatipController (Response (WithField "image" (Id "file") SearchQualificationModal))
controller _ = undefined