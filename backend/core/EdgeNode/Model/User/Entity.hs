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
import TH.InstanceBuilder
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Data.Aeson
import Data.Swagger
import Data.Scientific as Scientific
import Data.Maybe
import Data.Proxy

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
 
instance ToJSON UserId where
  toJSON (UserId i) = Number (Scientific.scientific (fromIntegral i) 0)

instance FromJSON UserId where
    parseJSON  = 
     withScientific "UserId" $ 
       fmap (fromMaybe err) 
     . traverse (return . UserId) 
     . Scientific.toBoundedInteger
     where err = error "json parser: userId" 

instance ToSchema UserId where
  declareNamedSchema _ = do
    schema <- declareSchema (Proxy :: Proxy Int)
    return $ NamedSchema (Just "UserId") schema

deriveAutoKey ''User
deriveWrappedPrimitivePersistField ''UserId