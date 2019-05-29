{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Config 
       (Config
       , db
       , ports
       , pass
       , port
       , database
       , host
       , user
         -- * load config
       , load  
       ) 
       where

import Data.Aeson    
import Data.Aeson.TH.Extended
import Control.Lens
import Control.Exception
import Data.Yaml

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

data Config = 
     Config 
     { configDb :: Db 
     , configPorts :: Ports
     } deriving Show

makeFields ''Config
makeFields ''Ports
makeFields ''Db

-- Load program configuration from file (server.yaml), or
-- raise YamlException and terminate program.
load :: FilePath -> IO Config
load path = decodeFileEither path >>= either throwIO pure

deriveFromJSON defaultOptions ''Db
deriveFromJSON defaultOptions ''Config