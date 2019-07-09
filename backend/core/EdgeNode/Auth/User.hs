{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module EdgeNode.Auth.User (User(..)) where

import EdgeNode.Model.User.Entity (UserId)

import qualified Crypto.JWT as Jose
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Servant.Auth.Server

data User = 
     User
     { userId         :: !UserId
     , userRoles      :: ![T.Text]
     , userEmail      :: !T.Text
     , userFirstName  :: !T.Text
     , userLastName   :: !T.Text
     , userMiddleName :: !T.Text
     } deriving Show
       deriving stock Generic

instance FromJSON User where
  parseJSON = withObject "user" $ \o -> do
    userId <- o .: "id"
    userRoles <- o .: "roles"
    userEmail <- o .: "email"
    userFirstName <- o .: "firstName"
    userLastName <- o .: "lastName"
    userMiddleName <- o .: "middleName"
    pure User{..}

instance ToJSON User where
  toJSON User{userId} = object ["id" .= userId]

instance FromJWT User where
  decodeJWT m = case fromJSON obj of
     Error e -> Left $ T.pack $ "Unable to decode: " ++ show e
     Success v -> pure v
    where
      obj = Object $ m ^. Jose.unregisteredClaims

instance ToJWT User where