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

module EdgeNode.Model.User.Entity (User, UserConstructor (..), Field (..), UserId (..), JWTUser (..)) where

import EdgeNode.User

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.ByteString
import Database.AutoKey
import TH.Instance
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Servant.Auth.Server

data User =
     User
     {  userEmail    :: !String
      , userPassword :: !ByteString
     }


instance FromJWT JWTUser where
  decodeJWT = undefined
    
instance ToJWT JWTUser where

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