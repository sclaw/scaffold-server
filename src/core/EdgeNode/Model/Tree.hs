{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

----------------------------------------------------------------------------

-- Module      :  Model.Tree
-- Maintainer  :  fclaw007@gmail.com
--
-- The "Model.Tree" rose-tree model
-----------------------------------------------------------------------------

module EdgeNode.Model.Tree (Tree (..)) where

import Database.Groundhog.Postgresql ()
import Database.Groundhog.TH.Extended
import Control.Lens
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Tree
import TH.Mk
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Control.Lens.Iso.Extended

mkPrimitivePersistFieldParam ''Tree [| jsonb |]
  