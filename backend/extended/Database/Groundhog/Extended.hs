{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Groundhog.Extended
  ( module Database.Groundhog
  , contains
  ) where
import Control.Lens (from, view)
import Control.Lens.Iso.Extended (stext)
import Data.Char
import Data.Semigroup ((<>))
import Database.Groundhog
import Database.Groundhog.Generic.Sql.Functions (like, lower)

contains x needle
  = let string u = map toLower $ view (from stext) u
    in  lower x `like` ("%" <> string needle <> "%")
