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
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Model.User  
       ( AuthenticatedUser (..)
       , AuthenticatedUserConstructor (..)
       , Field (..)
       , User (..)
       , UserId (..)
       , UserConstructor (..)
       , UserTablesBonds
       , defUser
       , isoUserGender
       , fromUserGender
       , toUserGender
       , coercedUserGender
       , hasqlEnumUserGender
       ) where

import EdgeNode.User

import Database.AutoKey
import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.ByteString
import TH.Mk
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import qualified Data.Text as T
import Orm.PersistField ()
import Data.Default.Class.Extended
import Data.Swagger
import Orphan ()
import Control.Lens
import Proto3.Suite.Types
import Data.Either
import Control.Lens.Iso.Extended

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

instance Default User_Gender where
  def = toEnum 0   
  
instance Default User
instance ToParamSchema UserId 

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
mkToSchemaAndJSONProtoIdent ''UserId
mkWrappedPrimitivePersistField ''UserId
mkFromHttpApiDataIdent ''UserId
mkEnumConvertor ''User_Gender
mkPrimitivePersistField ''User_Gender [| iso fromUserGender toUserGender |] 

defUser :: User
defUser = def

coercedUserGender :: Enumerated User_Gender -> User_Gender
coercedUserGender x = x^.(coerced :: Iso' (Enumerated User_Gender) (Either Int User_Gender)).to (fromRight undefined)

hasqlEnumUserGender :: T.Text -> Maybe User_Gender
hasqlEnumUserGender = Just . toUserGender . (^.from stext)