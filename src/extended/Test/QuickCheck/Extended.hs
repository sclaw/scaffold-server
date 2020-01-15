
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.QuickCheck.Extended
       ( module Test.QuickCheck
       , UnicodeText(..)
       , genText
       , genTextN
      ) where

import qualified Data.Text as T
import Test.QuickCheck

-- | Generate arbitrary text.
genText :: Gen T.Text
genText = T.pack . getPrintableString <$> arbitrary

-- | Get string of the bounded size.
genTextN :: Int -> Gen T.Text
genTextN n = T.pack . take (n - 1) . getPrintableString <$> arbitrary

-- | Wrapper for text in order to generate valid unicode string
newtype UnicodeText = UnicodeText T.Text

instance Arbitrary UnicodeText where
  arbitrary = UnicodeText <$> genText