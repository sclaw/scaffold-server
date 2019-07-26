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
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DerivingStrategies     #-}

module EdgeNode.Model.User.Entity  
       ( AuthenticatedUser (..)
       , AuthenticatedUserConstructor (..)
       , Field (..)
       , UserIdWrapper (..)
       , User (..)
       , UserConstructor (..)
       , UserKeyRel
       , wrapId
       ) where

import EdgeNode.User

import Database.AutoKey
import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.ByteString
import TH.Instance
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Data.Int (Int64)
import qualified Data.Text as T
import Orm.PersistField ()

data AuthenticatedUser =
     AuthenticatedUser
     { authenticatedUserEmail    :: !T.Text
     , authenticatedUserPassword :: !ByteString
     }

data UserKeyRel =
     UserKeyRel 
     { userKeyRelAuth     :: 
       DefaultKey 
       AuthenticatedUser
     , userKeyRelEdgeNode :: 
       DefaultKey User
     }

mkPersist_ [groundhog| 
 - entity: AuthenticatedUser
   schema: auth
   constructors:
    - name: AuthenticatedUser
      uniques: 
       - name: authenticatedUser_email_uk
         type: constraint
         fields: [authenticatedUserEmail]
 - entity: User
   schema: edgeNode
 - entity: UserKeyRel
   schema: edgeNode          
 |]
 
deriveAutoKey ''User
deriveToSchemaAndJSONProtoIdent ''UserId
deriveWrappedPrimitivePersistField ''UserId
deriveWrappedPrimitivePersistField ''UserIdWrapper

wrapId :: Int64 -> UserIdWrapper
wrapId = UserIdWrapper . UserId
