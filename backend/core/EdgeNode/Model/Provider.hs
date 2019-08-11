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

module EdgeNode.Model.Provider 
       ( Provider (..)
       , XProvider (..)
       , ProviderId (..)
       , Key (..)
       , autokey
       ) where

import EdgeNode.Provider

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core 
import Database.AutoKey
import TH.Generator
import Database.Groundhog.Generic

mkPersist_ [groundhog| 
 - entity: Provider
   schema: edgeNode
 |]

deriveWrappedPrimitivePersistField ''ProviderId
deriveAutoKey ''Provider