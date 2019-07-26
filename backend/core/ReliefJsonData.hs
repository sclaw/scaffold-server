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

-- | A wrapper for the either that is only applicative and has serialization that
-- is useful for us.
module ReliefJsonData (Alternative(..), eitherToAlt, Error (..)) where

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
  parseJSON = withObject "a" $ \v -> asum @[] [ Error <$> v .: "error", Fortune <$> v.: "success"]

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

data Error a = ServerError InternalServerError | ResponseError a

