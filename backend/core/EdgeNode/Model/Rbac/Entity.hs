{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TransformListComp      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module EdgeNode.Model.Rbac.Entity
       ( RoleTree
       , RoleId
       , Field (..)
       , RoleIdWrapper (..)
       )
       where

import EdgeNode.Rbac
import EdgeNode.Model.User.Entity  
import EdgeNode.Model.Tree

import Orm.PersistField ()
import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Control.Lens.Extended
import TH.Instance
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Data.Time.Clock
import Data.Aeson
import qualified Data.ByteString.Lazy as B

data RoleTree = 
     RoleTree 
     { roleTreeTree :: !(Tree Role)
     , roleTreeWho  :: !UserIdWrapper
     , roleTreeWhen :: !UTCTime 
     }

isoRoleTree :: Iso' Role B.ByteString
isoRoleTree = iso encode (either err id `fmap` eitherDecode) 
  where err = error . (<>) "tree decode error: "

derivePrimitivePersistField ''Role [| isoRoleTree |]
  
mkPersist_ [groundhog|
 - entity: RoleTree
   schema: main
 |]

deriveWrappedPrimitivePersistField ''RoleId
deriveToSchemaAndJSONProtoIdent ''RoleId