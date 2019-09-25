{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.Aeson.WithField.Extended ( module Data.Aeson.WithField, Or (..) ) where

import Data.Aeson.Extended
import Data.Aeson.WithField
import GHC.Generics
import Data.Foldable

newtype Or a b = Or (Either a b)
  deriving stock Generic
  deriving stock Show

instance (FromJSON a, FromJSON b) => FromJSON (Or a b) where
  parseJSON x = Or <$> asum [Left <$> parseJSON x, Right <$> parseJSON x]