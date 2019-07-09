{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Database.Exception (Db (..)) where

import Control.Exception.Base ()   
import Control.Exception.Hierarchy

data Db = Groundhog deriving Show

exceptionHierarchy Nothing (ExType ''Db)