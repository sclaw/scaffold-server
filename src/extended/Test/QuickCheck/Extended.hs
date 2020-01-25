
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.QuickCheck.Extended
       ( module Test.QuickCheck
       , genText 
       , genTextN
      ) where

import qualified Data.Text as T
import Test.QuickCheck
import Data.Aeson.Unit

-- | Generate arbitrary text.
genText :: Gen T.Text
genText = T.pack . getPrintableString <$> arbitrary

-- | Get string of the bounded size.
genTextN :: Int -> Gen T.Text
genTextN n = T.pack . take (n - 1) . getPrintableString <$> arbitrary

instance Arbitrary Unit where arbitrary = pure (toEnum 0)
