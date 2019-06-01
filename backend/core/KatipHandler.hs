{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module KatipHandler
       ( Config (..)
       , KatipHandler (..)
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

data KatipEnv = 
     KatipEnv 
     { katipEnvTerminal :: !Term
     , katipEnvOrmDB    :: !(Pool.Pool Postgresql)
     , katipEnvRawDB    :: !(Pool.Pool Hasql.Pool)
     }

data Config =
     Config
      { configNm       :: !Namespace
      , configCtx      :: !LogContexts
      , configEnv      :: !LogEnv
      , configKatipEnv :: !KatipEnv
      }

newtype KatipHandler a = KatipHandler { runKatipHandler :: ReaderT Config Handler a }
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
     )

makeFields ''Config
makeFields ''KatipEnv

-- These instances get even easier with lenses!
instance Katip KatipHandler where
    getLogEnv = KatipHandler $ asks configEnv
    localLogEnv f (KatipHandler m) = KatipHandler (local (over env f) m)

instance KatipContext KatipHandler where
    getKatipContext = KatipHandler $ asks configCtx
    localKatipContext f (KatipHandler m) = KatipHandler (local (over ctx f) m)
    getKatipNamespace = KatipHandler $ asks configNm
    localKatipNamespace f (KatipHandler m) = KatipHandler (local (over nm f) m)