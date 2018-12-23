{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module KatipHandler
       ( Config (..)
       , KatipHandler (..)
        -- * lens
       , nm
       , ctx
       , env
       ) where

import           Control.Lens
import           Control.Lens.TH            (makeFields)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Katip
import           Servant.Server             (Handler)
import           Control.Monad.Reader.Class (MonadReader)


data Config =
     Config
      { configNm  :: Namespace
      , configCtx :: LogContexts
      , configEnv :: LogEnv
      }

makeFields ''Config

newtype KatipHandler a = KatipHandler { runKatipHandler :: ReaderT Config Handler a }
    deriving (Functor, Applicative, Monad, MonadIO, (MonadReader Config))

-- These instances get even easier with lenses!
instance Katip KatipHandler where
    getLogEnv = KatipHandler $ asks configEnv
    localLogEnv f (KatipHandler m) = KatipHandler (local (over env f) m)

instance KatipContext KatipHandler where
    getKatipContext = KatipHandler $ asks configCtx
    localKatipContext f (KatipHandler m) = KatipHandler (local (over ctx f) m)
    getKatipNamespace = KatipHandler $ asks configNm
    localKatipNamespace f (KatipHandler m) = KatipHandler (local (over nm f) m)