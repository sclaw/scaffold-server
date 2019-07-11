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
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module EdgeNode.Model.User.Entity (User, UserConstructor (..), Field (..), UserId (..)) where

import EdgeNode.Api.User.UserId

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.ByteString
import Database.AutoKey
import TH.Instance
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)

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
deriveWrappedPrimitivePersistField ''UserId
deriveToSchemaAndJSONProtoIdent ''UserId