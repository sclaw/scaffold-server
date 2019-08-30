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
{-# LANGUAGE MultiParamTypeClasses #-}

module EdgeNode.Model.User.Trajectory 
       (module EdgeNode.User.Trajectory
       ) where

import EdgeNode.User.Trajectory

import Control.Lens.Iso.Extended
import Orm.PersistField ()
import Orphan ()
import TH.Mk
import Database.Groundhog.Generic 
       ( primToPersistValue
       , primFromPersistValue)

mkPrimitivePersistField ''QualificationDiff [| jsonb |]
mkWrappedPrimitivePersistField ''TrajectoryId