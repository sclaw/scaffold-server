{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Database.Exception (Groundhog (..), Hasql(..), _Request) where

import Control.Exception.Base ()   
import Control.Exception.Hierarchy
import qualified Data.ByteString as B
import Data.Typeable
import Hasql.Pool
import Data.Word (Word32)
import Control.Lens
import qualified Crypto.JOSE.Error as Jose
import qualified Crypto.JWT as Jose

data Groundhog e =  
       MigrationNotFound Word32 
     | MigrationSqlEmpty Word32
     | JWSError Jose.Error
     | JWTError Jose.JWTError
     | Common String
     | Action String
     | Request e
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
  
exceptionHierarchy True Nothing (ExType ''Groundhog)
makeClassyPrisms ''Groundhog
exceptionHierarchy False Nothing (ExType ''Hasql)

instance Jose.AsError (Groundhog a) where
  _Error = _JWSError

instance Jose.AsJWTError (Groundhog a) where
  _JWTError = _JWTError