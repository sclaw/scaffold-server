{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Config 
       ( Config
       , GroundhogSettings (..)
       , Db
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
         -- * load config
       , load  
       ) 
       where

import Data.Aeson    
import Data.Aeson.TH.Extended
import Control.Lens
import Control.Exception
import Data.Yaml
import Data.Time.Clock

data Db = Db 
     { dbHost :: String
     , dbPort :: Int
     , dbUser :: String
     , dbPass :: String
     , dbDatabase :: String
     } deriving Show

newtype Ports = Ports { portsPort :: Int } 
  deriving Show 
  deriving newtype FromJSON

newtype GroundhogSettings = GroundhogSettings { groundhogSettingsPoolN :: Int } 
  deriving Show 
  deriving newtype FromJSON

data HasqlSettings = HasqlSettings { hasqlSettingsPoolN :: Int, hasqlSettingsTm :: NominalDiffTime }
  deriving Show

data PoolSettings = 
     PoolSettings 
     { poolSettingsStripesN :: Int
     , poolSettingsTimeToOpen :: NominalDiffTime
     , poolSettingsResPerStripe :: Int 
     } deriving Show

data Config = 
     Config 
     { configDb :: Db 
     , configPorts :: Ports
     , configOrm :: GroundhogSettings
     , configRaw :: HasqlSettings
     , configPool :: PoolSettings
     } deriving Show

makeFields ''Config
makeFields ''Ports
makeFields ''Db
makeFields ''HasqlSettings
makeFields ''PoolSettings

-- Load program configuration from file (server.yaml), or
-- raise YamlException and terminate program.
load :: FilePath -> IO Config
load path = decodeFileEither path >>= either throwIO pure

deriveFromJSON defaultOptions ''Db
deriveFromJSON defaultOptions ''Config
deriveFromJSON defaultOptions ''HasqlSettings
deriveFromJSON defaultOptions ''PoolSettings