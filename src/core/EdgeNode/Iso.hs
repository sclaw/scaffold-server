{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module EdgeNode.Iso (country) where

import EdgeNode.Country

import Proto
import Control.Lens
import qualified Data.Text as T
import Proto3.Suite.Types
import Control.Lens.Iso.Extended

country :: Iso' (Enumerated Country) T.Text
country =  iso from' to'
  where from' = (^.(coerced :: Iso' (Enumerated Country) (Either Int Country))._Right.isoCountry.stext) 
        to' x = Enumerated $ Right (x^.from stext.from isoCountry)