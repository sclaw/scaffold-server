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
{-# LANGUAGE DefaultSignatures #-}

module EdgeNode.Model.User.Entity  
       ( AuthenticatedUser (..)
       , AuthenticatedUserConstructor (..)
       , Field (..)
       , User (..)
       , UserId (..)
       , UserConstructor (..)
       , UserTablesBonds
       , defUser
       ) where

import EdgeNode.User

import Database.AutoKey
import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.ByteString
import TH.Generator
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import qualified Data.Text as T
import Orm.PersistField ()
import Data.Default.Class

data AuthenticatedUser =
     AuthenticatedUser
     { authenticatedUserEmail    :: !T.Text
     , authenticatedUserPassword :: !ByteString
     }

data UserTablesBonds =
     UserTablesBonds 
     { userTablesBondsAuth     :: 
       DefaultKey 
       AuthenticatedUser
     , userTablesBondsEdgeNode :: 
       DefaultKey User
     }

instance Default User

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
 - entity: UserTablesBonds
   schema: edgeNode          
 |]
 
deriveAutoKey ''User
deriveToSchemaAndJSONProtoIdent ''UserId
deriveWrappedPrimitivePersistField ''UserId

defUser :: User
defUser = def