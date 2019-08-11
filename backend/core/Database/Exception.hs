{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DerivingStrategies #-}

module Database.Exception (Groundhog (..), Hasql(..)) where

import Control.Exception.Base ()   
import Control.Exception.Hierarchy
import qualified Data.ByteString as B
import Data.Typeable
import Hasql.Pool
import Data.Word (Word32)
import Control.Lens
import qualified Crypto.JOSE.Error as Jose
import qualified Crypto.JWT as Jose

data Groundhog =  
       MigrationNotFound Word32 
     | MigrationSqlEmpty Word32
     | JWSError Jose.Error
     | JWTError Jose.JWTError
     | Common String
     | Action String
     deriving Show

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
makeClassyPrisms ''Groundhog

instance Jose.AsError Groundhog where
  _Error = _JWSError

instance Jose.AsJWTError Groundhog where
  _JWTError = _JWTError