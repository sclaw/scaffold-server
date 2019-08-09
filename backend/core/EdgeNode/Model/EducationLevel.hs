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

module EdgeNode.Model.EducationLevel 
       ( StateExam
       , XStateExam (..)
       , HigherDegree
       , XHigherDegree (..)
       , InternationalDiploma
       , XInternationalDiploma (..)
       , LanguageStandard
       , XLanguageStandard (..)
       , autokey
       ) 
       where

import EdgeNode.Qualification

import Database.Groundhog.Postgresql ()
import Database.Groundhog.TH.Extended
import TH.Generator
import Database.Groundhog.Generic 
       ( primToPersistValue
       , primFromPersistValue)
import Control.Lens.Iso.Extended
import Database.AutoKey
import Data.Default.Class.Extended

instance Default XStateExam
instance Default XHigherDegree
instance Default XInternationalDiploma
instance Default XLanguageStandard

mkPersist_ [groundhog| 
 - entity: StateExam
   schema: edgeNode
 - entity: HigherDegree
   schema: edgeNode  
 - entity: InternationalDiploma
   schema: edgeNode
 - entity: LanguageStandard
   schema: edgeNode  
 |]

derivePrimitivePersistField ''Grade [| jsonb |]
deriveAutoKey ''StateExam
deriveAutoKey ''HigherDegree
deriveAutoKey ''InternationalDiploma
deriveAutoKey ''LanguageStandard