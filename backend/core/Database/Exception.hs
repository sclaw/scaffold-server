{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DerivingStrategies #-}

module Database.Exception (Groundhog (..), Hasql(..)) where

import Control.Exception.Base ()   
import Control.Exception.Hierarchy
import qualified Data.ByteString as B
import Data.Typeable
import Hasql.Pool
import Data.Word (Word16)

data Groundhog = MigrationNotFound Word16 deriving Show

data Hasql = 
      ForeignKeyViolation 
      !B.ByteString
    | UniqueViolation 
      !B.ByteString
    | OtherError 
      !UsageError
    deriving Typeable
    deriving Show
  
exceptionHierarchy Nothing (ExType ''Groundhog)
exceptionHierarchy Nothing (ExType ''Hasql)