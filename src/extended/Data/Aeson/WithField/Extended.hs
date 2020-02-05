{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Aeson.WithField.Extended
      ( module Data.Aeson.WithField
      , OptField(..)
      , Or (..)
      , pattern JustOptField
      , pattern NoOptField
      , pattern AnyOptField
      ) where

import Data.Aeson
import Data.Aeson.WithField
import qualified Data.Text as T
import Data.Proxy
import Data.Swagger
import qualified Data.HashMap.Strict as HM
import Test.QuickCheck
import GHC.Generics
import Data.Foldable
import GHC.TypeLits

import Debug.Trace


instance (Arbitrary a, Arbitrary b) => Arbitrary (WithField s a b) where
  arbitrary = WithField <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (OnlyField s a) where
  arbitrary = OnlyField <$> arbitrary
 
newtype OptField s a v = OptField (WithField s (Maybe a) v)
  deriving Generic
  deriving newtype ToJSON
  deriving newtype Arbitrary
  deriving anyclass ToSchema
  deriving stock Show
  deriving stock Eq

instance (KnownSymbol s, FromJSON a, FromJSON v) =>
         FromJSON (OptField s a v) where
  parseJSON = withObject "opt-field" $ \o -> do
    traceM (show (o, field))
    a <- o .:? field
    _ <- undefined
    v <- parseJSON $ Object $ HM.delete field o
    pure $ OptField $ WithField a v
    where field = T.pack $ symbolVal (Proxy :: Proxy s)

newtype Or a b = Or (Either a b)
  deriving stock Generic
  deriving stock Show
  deriving newtype Arbitrary
  deriving anyclass ToSchema
  deriving stock Eq
  
instance (FromJSON a, FromJSON b) => FromJSON (Or a b) where
  parseJSON x = Or <$> asum [Left <$> parseJSON x, Right <$> parseJSON x]

instance (ToJSON a, ToJSON b) => ToJSON (Or a b) where
  toJSON (Or (Left x)) = toJSON x
  toJSON (Or (Right x)) = toJSON x

pattern JustOptField :: a -> b -> OptField s a b
pattern JustOptField x y = OptField (WithField (Just x) y)
pattern NoOptField :: b -> OptField s a b
pattern NoOptField y = OptField (WithField Nothing y)
pattern AnyOptField :: Maybe a -> b -> OptField s a b
pattern AnyOptField x y = OptField (WithField x y)