{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module KatipController
       ( Config (..)
       , KatipController (..)
       , KatipEnv (..)
       , KatipLoggerIO
         -- * lens
       , nm
       , ctx
       , env
       , katipEnv
       , terminal
       , ormDB
       , rawDB
         -- * run
       , runKatipController  
       ) where

import EdgeNode.Rbac
import EdgeNode.Model.Tree

import Control.Lens
import Control.Lens.TH (makeFields)
import Control.Monad.IO.Class
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.RWS.Strict
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
         
type KatipLoggerIO = Severity -> LogStr -> IO ()

data KatipEnv = 
     KatipEnv 
     { katipEnvTerminal :: !Term
     , katipEnvOrmDB    :: !(Pool.Pool Postgresql)
     , katipEnvRawDB    :: !Hasql.Pool
     }

data Config =
     Config
      { configNm       :: !Namespace
      , configCtx      :: !LogContexts
      , configEnv      :: !LogEnv
      , configKatipEnv :: !KatipEnv
      }

newtype State = State { stateRoles :: Maybe (Tree Role) }

newtype KatipController a = KatipController { unwrap :: RWST Config [String] State Handler a }
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad
  deriving newtype MonadIO
  deriving newtype (MonadReader Config)
  deriving newtype (MonadState State)
  deriving newtype (MonadWriter [String])
  deriving newtype (MonadRWS Config [String] State)
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

defState :: State
defState = State Nothing

runKatipController :: Config -> KatipController a -> Handler a
runKatipController config app = fst `fmap` evalRWST (unwrap app) config (State Nothing)
