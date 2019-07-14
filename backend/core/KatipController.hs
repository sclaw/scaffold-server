{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module KatipController
       ( Config (..)
       , KatipController (..)
       , KatipEnv (..)
        -- * lens
       , nm
       , ctx
       , env
       , katipEnv
       , terminal
       , ormDB
       , rawDB
       ) where

import           Control.Lens
import           Control.Lens.TH               (makeFields)
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class    (MonadReader)
import           Control.Monad.Trans.Reader
import qualified Data.Pool                     as Pool
import           Database.Groundhog.Postgresql (Postgresql)
import           Katip
import           Servant.Server                (Handler)
import           Control.Monad.Trans.Control   (MonadBaseControl)
import           Control.Monad.Base            (MonadBase)
import           Control.Monad.Error.Class
import           Servant.Server.Internal.ServantErr
import           Data.Monoid.Colorful          (Term)
import qualified Hasql.Pool                    as Hasql
import           Control.Monad.Catch           hiding (Handler) 
import           Control.Exception.Safe       (MonadMask)

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

newtype KatipController a = KatipController { runKatipController :: ReaderT Config Handler a }
    deriving
     ( Functor
     , Applicative
     , Monad
     , MonadIO
     , (MonadReader Config)
     , MonadBase IO
     , MonadBaseControl IO
     , MonadError ServantErr
     , MonadCatch
     , MonadThrow
     , MonadMask 
     )

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