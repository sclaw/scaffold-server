{-# LANGUAGE TemplateHaskell #-}

module Orm.PersistField.Enumerated () where

import Orphan ()
import Proto3.Suite.Types
import TH.Mk
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Database.Groundhog.Instances ()
import Control.Lens.Iso.Extended

mkPrimitivePersistFieldParam ''Enumerated [| enumtext |]