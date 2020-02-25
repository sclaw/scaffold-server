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
       ( Permission (..)
       , Role (..)
       , isoPermission
       , isoRole
       )
       where

import TH.Mk
import Control.Lens 
import Test.QuickCheck
import Database.Transaction (ParamsShow (..))
import Test.QuickCheck.Arbitrary.Generic
import GHC.Generics

data Permission = 
       PermissionRoot
     | PermissionProviderAdmin
     | PermissionProviderGuest
     | PermissionUser 
     deriving stock Eq
     deriving stock Show
     deriving stock Generic

data Role = 
       RoleRoot 
     | RoleUser
     | RoleProvider
     | RoleGuest
     deriving stock Eq
     deriving stock Show
     deriving stock Generic

mkEnumConvertor ''Permission
mkEnumConvertor ''Role 

instance Arbitrary Role where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Permission where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance ParamsShow Permission where render = (^.isoPermission)
instance ParamsShow Role where render = (^.isoRole)