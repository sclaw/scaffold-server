{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
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
       , KatipControllerState (..)
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
       , jwk
         -- * run
       , runKatipController
         -- * re-export
       , module R  
       ) where

-- import EdgeNode.Rbac
-- import EdgeNode.Model.Tree

import Control.Lens
import Control.Lens.TH (makeFields)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Reader.Class as R
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
import Crypto.JOSE.JWK
import Control.Monad.Time
import Data.Time
import qualified Control.Monad.RWS.Strict as RWS 
import Control.Monad.RWS.Class
import Control.Concurrent.MVar
import Crypto.JWT (JWTError)

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
     , katipEnvJwk :: !JWK     
     }

newtype KatipLogger = KatipWriter [String] 
  deriving newtype Monoid
  deriving newtype Semigroup  
  deriving newtype NFData   

newtype KatipState = KatipState Int
  deriving newtype Default
  deriving newtype NFData

data Config =
     Config
      { configNm       :: !Namespace
      , configCtx      :: !LogContexts
      , configEnv      :: !LogEnv
      , configKatipEnv :: !KatipEnv
      }

instance MonadTime Handler where
  currentTime = liftIO getCurrentTime

newtype KatipControllerState = KatipControllerState { jwtError :: MVar JWTError }

newtype KatipControllerWriter = KatipControllerWriter [String]
  deriving newtype Monoid
  deriving newtype Semigroup  

newtype KatipController a = 
        KatipController 
        { unwrap 
          :: RWS.RWST Config 
             KatipControllerWriter
             KatipControllerState 
             Handler a 
        }
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad
  deriving newtype MonadIO
  deriving newtype (MonadReader Config)
  deriving newtype (MonadState KatipControllerState)
  deriving newtype (MonadWriter KatipControllerWriter)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype (MonadError ServantErr)
  deriving newtype MonadCatch
  deriving newtype MonadThrow
  deriving newtype MonadMask 
  deriving newtype MonadTime
  deriving newtype (MonadRWS Config KatipControllerWriter KatipControllerState)
  
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

runKatipController :: Config -> KatipControllerState -> KatipController a -> Handler a
runKatipController cfg st app = fmap fst (RWS.evalRWST (unwrap app) cfg st)