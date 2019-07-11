{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module EdgeNode.Auth.JWTUser 
       ( JWTUser
       , jWTUserIdent
       , jWTUserRoles
       , jWTUserEmail
       ) where

import EdgeNode.Api.User.JWTUser
import EdgeNode.Model.User.Entity (UserId(UserId))
import EdgeNode.Model.Rbac.Entity ()

import Servant.Auth.Server
import Swagger.Proto ()
import Data.Aeson
import Data.Swagger
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product
import Text.ProtocolBuffers.Basic (defaultValue)
import Data.Function ((&))
import Control.Lens.Iso.Extended

instance FromJWT JWTUser where
  decodeJWT = undefined

instance ToJWT JWTUser where

-- |  ToJSON JWTUser
--
-- >>> encode (defaultValue :: JWTUser) 
--  "{\"id\":0, \"roles\":[], \"email\":\"\"}"
-- 
instance ToJSON JWTUser where
    toJSON x = 
      object 
      [ "id" .= toJSON (x^.field @"_jWTUserIdent")
      , "roles" .= toJSON (x^.field @"_jWTUserRoles")
      , "email" .= toJSON (x^.field @"_jWTUserEmail") ]

-- |  FromJSON JWTUser
--
-- >>> eitherDecode ("{\"id\":0, \"roles\":[], \"email\":\"\"}"^.stext.textbsl) :: Either String JWTUser 
--  Right (JWTUser {_jWTUserIdent = UserId {_userIdIdent = 0}, _jWTUserRoles = fromList [], _jWTUserEmail = ""})
-- 
instance FromJSON JWTUser where
  parseJSON = withObject "JWTUser" $ \obj -> 
    do ident <- obj .: "id"
       roles <- obj .: "roles"
       email <- obj .: "email"
       let val = 
             defaultValue 
             & field @"_jWTUserIdent" .~  UserId ident
             & field @"_jWTUserRoles" .~ roles^.seql
             & field @"_jWTUserEmail" .~ email^.sutf8
       return val

instance ToSchema JWTUser where
  declareNamedSchema _ = udefined