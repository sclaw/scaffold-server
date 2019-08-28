{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module EdgeNode.Config 
       ( Config
       , GroundhogSettings (..)
       , Katip (..)
       , Db
       , Auth (..)
       , Service (..)
       , ApiKeys (..)
       , Hosts (..)
       , db
       , ports
       , pass
       , port
       , database
       , host
       , user
       , orm
       , poolN
       , tm
       , raw
       , pool
       , stripesN
       , timeToOpen
       , resPerStripe
       , katip
       , ekg
         -- * load config
       , load
       , path
       , verbosity
       , severity  
       , env
       , auth
       , jwk
       , isAuthEnabled
       , service
       , hosts
       , userId
       ) 
       where

import Data.Aeson    
import Data.Aeson.TH.Extended
import Control.Lens
import Control.Exception
import Data.Yaml
import Data.Time.Clock
import Data.Int

data Db = Db 
     { dbHost :: !String
     , dbPort :: !Int
     , dbUser :: !String
     , dbPass :: !String
     , dbDatabase :: !String
     } deriving Show

newtype Ports = Ports { portsPort :: Int } 
  deriving Show 
  deriving newtype FromJSON

newtype Hosts = Hosts { hostsSwagger :: String }
  deriving Show 
  deriving newtype FromJSON

newtype GroundhogSettings = GroundhogSettings { groundhogSettingsPoolN :: Int } 
  deriving Show 
  deriving newtype FromJSON

data HasqlSettings = HasqlSettings { hasqlSettingsPoolN :: !Int, hasqlSettingsTm :: !NominalDiffTime }
  deriving Show

data PoolSettings = 
     PoolSettings 
     { poolSettingsStripesN :: !Int
     , poolSettingsTimeToOpen :: !NominalDiffTime
     , poolSettingsResPerStripe :: !Int 
     } deriving Show

data Katip = 
     Katip 
     { katipPath :: !FilePath
     , katipSeverity :: !String
     , katipVerbosity :: !String
     , katipEnv :: !String
     } 
  deriving Show

data Ekg = Ekg { ekgHost :: !String, ekgPort :: !Int } deriving Show

data Auth = 
     Auth 
     { authJwk :: !FilePath
     , authIsAuthEnabled :: !Bool
     , authUserId :: !Int64 
     }
  deriving Show 

newtype ApiKeys = ApiKeys [(String, String)]
  deriving Show

newtype Service = Service { serviceApiKeys :: ApiKeys }
  deriving Show

data Config = 
     Config 
     { configDb :: !Db 
     , configPorts :: !Ports
     , configOrm :: !GroundhogSettings
     , configRaw :: !HasqlSettings
     , configPool :: !PoolSettings
     , configKatip :: !Katip
     , configEkg :: !Ekg
     , configAuth :: !Auth
     , configService :: !Service
     , configHosts :: !Hosts 
     } deriving Show

makeFields ''Config
makeFields ''Ports
makeFields ''Db
makeFields ''HasqlSettings
makeFields ''PoolSettings
makeFields ''Ekg
makeFields ''Katip
makeFields ''Auth

-- Load program configuration from file (server.yaml), or
-- raise YamlException and terminate program.
load :: FilePath -> IO Config
load path = decodeFileEither path >>= either throwIO pure

deriveFromJSON defaultOptions ''Db
deriveFromJSON defaultOptions ''Config
deriveFromJSON defaultOptions ''HasqlSettings
deriveFromJSON defaultOptions ''PoolSettings
deriveFromJSON defaultOptions ''Ekg
deriveFromJSON defaultOptions ''Katip
deriveFromJSON defaultOptions ''Auth
deriveFromJSON defaultOptions ''ApiKeys
deriveFromJSON defaultOptions ''Service