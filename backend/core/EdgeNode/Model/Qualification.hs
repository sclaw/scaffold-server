{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module EdgeNode.Model.Qualification 
       ( Qualification (..)
       , XQualification (..)
       , QualificationId (..)
       , Key (..)
       , autokey
       ) where

import EdgeNode.Qualification

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core 
import Database.AutoKey
import TH.Generator
import Database.Groundhog.Generic
import Control.Lens.Iso.Extended
import Data.Aeson.TH

mkPersist_ [groundhog| 
 - entity: Qualification
   schema: edgeNode
 |]

deriveWrappedPrimitivePersistField ''QualificationId
deriveAutoKey ''Qualification
deriveJSON defaultOptions ''QualificationGrade
derivePrimitivePersistField ''QualificationGrade [| jsonb |]