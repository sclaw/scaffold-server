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
import TH.Mk
import Database.Groundhog.Generic
import Orm.PersistField ()
import Orphan ()
import Data.Swagger.ParamSchema

mkPersist_ [groundhog| 
 - entity: Provider
   schema: edgeNode
 |]

instance ToParamSchema ProviderId

mkWrappedPrimitivePersistField ''ProviderId
deriveAutoKey ''Provider
mkFromHttpApiDataIdent ''ProviderId