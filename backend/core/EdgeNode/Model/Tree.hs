{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
----------------------------------------------------------------------------

-- Module      :  Model.Tree
-- Maintainer  :  fclaw007@gmail.com
--
-- The "Model.Tree" rose-tree model
-----------------------------------------------------------------------------

module EdgeNode.Model.Tree (Tree (..)) where

import Database.Groundhog.Postgresql ()
import Database.Groundhog.TH.Extended

import Data.Tree


mkPersist_ [groundhog|
definitions:
  - entity: Tree
|]