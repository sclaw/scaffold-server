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
import Data.DeriveTH

data Permission = 
       PermissionRoot
     | PermissionProviderAdmin
     | PermissionProviderGuest
     | PermissionUser 
     deriving stock Eq
     deriving stock Show

data Role = 
       RoleRoot 
     | RoleUser
     | RoleProvider
     | RoleGuest
     deriving stock Eq
     deriving stock Show

mkEnumConvertor ''Permission
mkEnumConvertor ''Role 
derive makeArbitrary ''Permission
derive makeArbitrary ''Role

instance ParamsShow Permission where render = (^.isoPermission) 
instance ParamsShow Role where render = (^.isoRole)