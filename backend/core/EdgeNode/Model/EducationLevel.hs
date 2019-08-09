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

module EdgeNode.Model.EducationLevel 
       ( StateExam
       , HigherDegree
       , InternationalDiploma
       , LanguageStandard
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