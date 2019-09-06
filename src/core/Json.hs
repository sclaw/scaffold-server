{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A wrapper for the either that is only applicative and has serialization that
-- is useful for us.
module Json 
       ( Alternative(..)
       , eitherToAlt
       , Error (..)
       , ValueWrapper (..)
       ) where

import EdgeNode.Error

import Data.Aeson hiding (Error, Success)
import Data.Proxy
import Data.Swagger hiding (ValidationError)
import GHC.Generics hiding (to)
import Control.Lens hiding ((.=))
import Data.Typeable
import Data.Foldable (asum)
import Data.Validation
import Control.Lens.Iso.Extended
import qualified Data.Text as T

-- | Analogue of the Either datatype.
data Alternative a b = Error a | Fortune b
     deriving (Generic, Functor, Foldable, Traversable, Show)

instance Applicative (Alternative a) where
  pure = Fortune
  Error e <*> _ = Error e
  Fortune f <*> x = fmap f x

instance Validate Alternative where
  _Validation =
    iso
     (\x -> _Validation # case x of
       Error e -> Failure e
       Fortune a -> Success a)
     (\x -> case x^._Validation of
       Failure e -> Error e
       Success a -> Fortune a)

instance (ToJSON a, ToJSON b) => ToJSON (Alternative a b) where
    toJSON (Error e) = object [ "error" .= toJSON e ]
    toJSON (Fortune x) = object [ "success" .= toJSON x ]

instance forall a b . (FromJSON a, FromJSON b) => FromJSON (Alternative a b) where
  parseJSON = withObject "Alternative" $ \v -> 
    asum @[] [ Error <$> v .: "error", Fortune <$> v.: "success"]

instance (Typeable a, ToSchema a, Typeable b, ToSchema b) => ToSchema (Alternative a b) where
  declareNamedSchema _ = do
    left <- declareSchemaRef (Proxy :: Proxy a)
    right <- declareSchemaRef (Proxy :: Proxy b)
    let leftUnique = typeOf (undefined :: a)^.to show.stext
    let rightUnique = typeOf (undefined :: b)^.to show.stext
    let name = "Alternative." <> leftUnique <> "." <> rightUnique
    let schema = 
         NamedSchema (Just name) $ mempty
         & type_ .~ SwaggerObject
         & properties .~ [("error", left), ("success", right)]
    return schema     

instance Bifunctor Alternative where 
  bimap f g = (^.eitherToAlt) . bimap f g . (^._Either)
    
eitherToAlt :: Iso' (Either e a) (Alternative e a)
eitherToAlt = iso from to 
  where from = either Error Fortune
        to (Error e) = Left e
        to (Fortune a) = Right a

data Error a = ServerError InternalServerError | ResponseError a | AuthError T.Text
  deriving stock Generic
  deriving stock Show

instance ToJSON a => ToJSON (Error a) where
  toJSON (ServerError e) = object [ "server" .= toJSON e ]
  toJSON (ResponseError e) = object [ "client" .= toJSON e ]
  toJSON (AuthError e) = object [ "auth" .= toJSON e ]

instance forall a . FromJSON a => FromJSON (Error a) where
  parseJSON = withObject "Error" $ \v -> 
    asum @[] 
     [ ServerError <$> v .: "server"
     , ResponseError <$> v.: "client"
     , AuthError <$> v.: "auth" ]

instance (Typeable a, ToSchema a) => ToSchema (Error a) where
  declareNamedSchema _ = do
    a <- declareSchemaRef (Proxy :: Proxy a)
    server <- declareSchemaRef (Proxy :: Proxy InternalServerError)
    auth <- declareSchemaRef (Proxy :: Proxy T.Text)
    let unique = typeOf (undefined :: a)^.to show.stext
    let name = "Error." <> unique
    let schema = 
          NamedSchema (Just name) $ mempty
          & type_ .~ SwaggerObject
          & properties .~ [("server", server), ("client", a), ("auth", auth)]
    return schema

newtype ValueWrapper = ValueWrapper Value
  deriving stock Generic
  deriving newtype ToJSON
  deriving newtype FromJSON