{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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

module EdgeNode.Model.Rbac
       ( RoleId (..)
       , RoleIdWrapper (..)
       , Permission (..)
       , isoPermission
       )
       where

import EdgeNode.Rbac 

import TH.Mk
import Control.Lens 
import Data.String
import Orm.PersistField ()
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Test.QuickCheck

instance Arbitrary RoleId where
  arbitrary = RoleId <$> arbitrary

data Permission = 
       Root
     | ProviderAdmin
     | ProviderGuest
     | User 
     deriving stock Eq
     deriving stock Show

instance Arbitrary Permission where
  arbitrary = oneof [pure Root, pure ProviderAdmin, pure ProviderGuest, pure User]

mkEnumConvertor ''Permission
mkToSchemaAndJSONProtoIdent ''RoleId
mkWrappedPrimitivePersistField ''RoleId

instance IsString Permission where
  fromString = toPermission