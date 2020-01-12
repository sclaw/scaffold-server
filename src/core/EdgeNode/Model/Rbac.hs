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
       , isoPermission
       )
       where

import TH.Mk
import Control.Lens 
import Test.QuickCheck
import Database.Transaction (ParamsShow (..))
import Data.String

data Permission = 
       Root
     | ProviderAdmin
     | ProviderGuest
     | User 
     deriving stock Eq
     deriving stock Show

instance Arbitrary Permission where
  arbitrary = oneof [pure Root, pure ProviderAdmin, pure ProviderGuest, pure User]

instance ParamsShow Permission where render = show 

mkEnumConvertor ''Permission

instance IsString Permission where
  fromString = toPermission