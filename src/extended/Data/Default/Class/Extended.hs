module Data.Default.Class.Extended (module Data.Default.Class) where

import Data.Default.Class
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

instance Default LT.Text where
  def = LT.empty

instance Default B.ByteString where
  def = B.empty

instance Default BL.ByteString where
  def = BL.empty