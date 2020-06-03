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

import EdgeNode.Transport.Rbac

import TH.Mk
import Control.Lens
import Database.Transaction

{-
roles:

                  root
          _________|_________
          |                  |
         user             provider
                             |
                           editor
                             |
                           guest
permissions:

                      root
              _________|_________
             |                   |
            user            providerAdmin
                          _______|_________
                          |               |
                    providerEditor   providerSettings
                          |
                    providerGuest
-}

mkEnumConvertor ''Permission
mkEnumConvertor ''Role
mkArbitrary ''Role
mkArbitrary ''Permission

instance ParamsShow Permission where render = (^.isoPermission)
instance ParamsShow Role where render = (^.isoRole)