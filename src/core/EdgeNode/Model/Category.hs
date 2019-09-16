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

module EdgeNode.Model.Category 
       ( StateExam (..)
       , XStateExam (..)
       , HigherDegree (..)
       , XHigherDegree (..)
       , InternationalDiploma (..)
       , XInternationalDiploma (..)
       , LanguageStandard (..)
       , XLanguageStandard (..)
       , autokey
       ) 
       where

import EdgeNode.Category

import Database.Groundhog.Postgresql ()
import Database.Groundhog.TH.Extended
import TH.Mk
import Database.Groundhog.Generic 
       ( primToPersistValue
       , primFromPersistValue)
import Control.Lens.Iso.Extended
import Database.AutoKey
import Data.Default.Class.Extended
import Orm.PersistField.Enumerated ()
import Orm.PersistField ()
import Orphan ()
import EdgeNode.Model.Provider ()

mkPrimitivePersistField ''LSGrade [| jsonb |]
mkWrappedPrimitivePersistField ''StateExamId
mkWrappedPrimitivePersistField ''HigherDegreeId
mkWrappedPrimitivePersistField ''InternationalDiplomaId
mkWrappedPrimitivePersistField ''LanguageStandardId