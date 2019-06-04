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
{-# LANGUAGE TransformListComp     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Model.Rbac.Entity
       ( Role
       , RoleTree
       , Field (..)
       , tree
       )
       where

import Api.Rbac.RoleId
import Api.Rbac.Role (Role)

import Orm.PersistField ()
import Model.Tree
import Database.AutoKey
import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Control.Lens.Extended
import TH.InstanceBuilder (deriveWrappedPrimitivePersistField)
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)

data RoleTree = RoleTree { roleTreeTree :: Tree RoleId }

mkPersist_ [groundhog|
 - entity: Role
 - entity: RoleTree
   uniques:
    - name: role_tree_uk
      type: constraint
      fields: [roleHierarchyTree]

 |]

deriveAutoKey ''Role
deriveWrappedPrimitivePersistField ''RoleId

makeFields ''RoleTree