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
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Model.User.Entity (User, UserConstructor (..), Field (..), UserId) where

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.ByteString
import Database.AutoKey
import Data.Int (Int64)
import TH.InstanceBuilder (deriveWrappedPrimitivePersistField)
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)

newtype UserId = UserId { userIdIdent :: Int64 }

data User =
     User
     {  userLogin    :: !String
      , userEmail    :: !String
      , userPassword :: !ByteString
     }

mkPersist_ [groundhog| 
 - entity: User
 |]
deriveAutoKey ''User
deriveWrappedPrimitivePersistField ''UserId