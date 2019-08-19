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
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE InstanceSigs  #-}

module EdgeNode.Model.Provider 
       ( Provider (..)
       , XProvider (..)
       , ProviderId (..)
       , Key (..)
       , SearchPiece (..)
       , autokey
       ) where

import EdgeNode.Provider

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core 
import Database.AutoKey
import TH.Generator
import Database.Groundhog.Generic
import Orm.PersistField ()
import Orphan ()
import GHC.Generics

data SearchPiece = All | Qualification 
  deriving stock Generic
  deriving stock Show
  deriving stock Read

mkPersist_ [groundhog| 
 - entity: Provider
   schema: edgeNode
 |]

deriveWrappedPrimitivePersistField ''ProviderId
deriveAutoKey ''Provider
mkParamSchemaEnum ''SearchPiece
mkFromHttpApiDataEnum ''SearchPiece