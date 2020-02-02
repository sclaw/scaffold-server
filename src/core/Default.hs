module Default () where

import Data.Default.Class.Extended
import Proto3.Suite.Types

instance Default a => Default (Enumerated a) where
  def = Enumerated $ Right def