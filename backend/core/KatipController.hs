{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}

module KatipController
       ( Config (..)
       , KatipController (..)
       , KatipEnv (..)
       , KatipLogger (..)
       , KatipState (..)
       , KatipLoggerIO
         -- * lens
       , nm
       , ctx
       , env
       , katipEnv
       , terminal
       , ormDB
       , rawDB
       , httpReqManager
       , apiKeys
         -- * run
       , runKatipController  
       ) where

-- import EdgeNode.Rbac
-- import EdgeNode.Model.Tree

import Control.Lens
import Control.Lens.TH (makeFields)
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Pool as Pool
import Database.Groundhog.Postgresql (Postgresql)
import Katip
import Servant.Server (Handler)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (MonadBase)
import Control.Monad.Error.Class
import Servant.Server.Internal.ServantErr
import Data.Monoid.Colorful (Term)
import qualified Hasql.Pool as Hasql
import Control.Monad.Catch hiding (Handler) 
import Control.Exception.Safe (MonadMask)
import Data.Default.Class
import Control.DeepSeq
import Network.HTTP.Client

type KatipLoggerIO = Severity -> LogStr -> IO ()

data KatipEnv = 
     KatipEnv 
     { katipEnvTerminal 
       :: !Term
     , katipEnvOrmDB    
       :: !(Pool.Pool Postgresql)
     , katipEnvRawDB    
       :: !Hasql.Pool
     , katipEnvHttpReqManager 
       :: !Manager
     , katipEnvApiKeys
       :: ![(String, String)]   
     }

newtype KatipLogger = AppLogger [String] 
  deriving newtype Monoid
  deriving newtype Semigroup  
  deriving newtype NFData   

newtype KatipState = AppState Int
  deriving newtype Default
  deriving newtype NFData

data Config =
     Config
      { configNm       :: !Namespace
      , configCtx      :: !LogContexts
      , configEnv      :: !LogEnv
      , configKatipEnv :: !KatipEnv
      }

newtype KatipController a = KatipController { unwrap :: ReaderT Config Handler a }
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad
  deriving newtype MonadIO
  deriving newtype (MonadReader Config)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype (MonadError ServantErr)
  deriving newtype MonadCatch
  deriving newtype MonadThrow
  deriving newtype MonadMask 
  
makeFields ''Config
makeFields ''KatipEnv

-- These instances get even easier with lenses!
instance Katip KatipController where
    getLogEnv = KatipController $ asks configEnv
    localLogEnv f (KatipController m) = KatipController (local (over env f) m)

instance KatipContext KatipController where
    getKatipContext = KatipController $ asks configCtx
    localKatipContext f (KatipController m) = KatipController (local (over ctx f) m)
    getKatipNamespace = KatipController $ asks configNm
    localKatipNamespace f (KatipController m) = KatipController (local (over nm f) m)

runKatipController :: Config -> KatipController a -> Handler a
runKatipController cfg app = runReaderT (unwrap app) cfg   