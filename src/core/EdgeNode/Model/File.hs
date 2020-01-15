{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EdgeNode.Model.File (Hash (..), Name (..), Mime (..), UnicodeText (..)) where

import Test.QuickCheck.Extended

newtype Hash = Hash UnicodeText deriving newtype Arbitrary

newtype Name = Name UnicodeText deriving newtype Arbitrary
 
newtype Mime = Mime UnicodeText deriving newtype Arbitrary