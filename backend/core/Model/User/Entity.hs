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

import Api.User.UserId

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.ByteString
import Database.AutoKey
import TH.InstanceBuilder (deriveWrappedPrimitivePersistField)
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)


data User =
     User
     {  userEmail    :: !String
      , userPassword :: !ByteString
     }

mkPersist_ [groundhog| 
 - entity: User
 |]
deriveAutoKey ''User
deriveWrappedPrimitivePersistField ''UserId