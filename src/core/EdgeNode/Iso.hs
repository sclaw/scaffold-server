{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module EdgeNode.Iso (country, language) where

import EdgeNode.Country
import EdgeNode.Lang

import TH.Proto
import Control.Lens
import qualified Data.Text as T
import Proto3.Suite.Types
import Control.Lens.Iso.Extended

country :: Iso' (Enumerated Country) T.Text
country =  iso from' to'
  where from' = (^.(coerced :: Iso' (Enumerated Country) (Either Int Country))._Right.isoCountry.stext) 
        to' x = Enumerated $ Right (x^.from stext.from isoCountry)

language :: Iso' (Enumerated Language) T.Text
language =  iso from' to'
  where from' = (^.(coerced :: Iso' (Enumerated Language) (Either Int Language))._Right.isoLanguage.stext) 
        to' x = Enumerated $ Right (x^.from stext.from isoLanguage)