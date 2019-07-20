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
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module EdgeNode.Model.User.Entity  
       ( User
       , UserConstructor (..)
       , Field (..)
       , UserIdWrapper (..)
       , wrapId
       ) where

import EdgeNode.UserId

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.ByteString
import Database.AutoKey
import TH.Instance
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Data.Int (Int64)

data User =
     User
     {  userEmail    :: !String
      , userPassword :: !ByteString
     }

mkPersist_ [groundhog| 
 - entity: User
   schema: main
   constructors:
    - name: User
      uniques: 
       - name: user_userEmail_uk
         type: constraint
         fields: [userEmail]
 |]
 
deriveAutoKey ''User
deriveToSchemaAndJSONProtoIdent ''UserId
deriveWrappedPrimitivePersistField ''UserId
deriveWrappedPrimitivePersistField ''UserIdWrapper

wrapId :: Int64 -> UserIdWrapper
wrapId = UserIdWrapper . UserId
