{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module EdgeNode.Config 
       ( Config
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
       , poolN
       , tm
       , hasql
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
       , accessKey
       , secretKey
       , amazon
       , bucketPrefix
       ) 
       where

import Data.Aeson    
import Data.Aeson.TH.Extended
import Control.Lens
import Control.Exception
import Data.Yaml
import Data.Time.Clock
import Data.Int
import qualified Network.AWS as AWS
import qualified Data.Text as T

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

data HasqlSettings = 
     HasqlSettings 
     { hasqlSettingsPoolN :: !Int
     , hasqlSettingsTm :: !NominalDiffTime
     , hasqlSettingsResPerStripe :: !Int 
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

deriving instance Show AWS.SecretKey

data Amazon = 
     Amazon 
     { amazonAccessKey :: !AWS.AccessKey
     , amazonSecretKey :: !AWS.SecretKey
     , amazonBucketPrefix :: !T.Text 
     }
  deriving Show

data Config = 
     Config 
     { configDb :: !Db 
     , configPorts :: !Ports
     , configHasql :: !HasqlSettings
     , configKatip :: !Katip
     , configEkg :: !Ekg
     , configAuth :: !Auth
     , configService :: !Service
     , configHosts :: !Hosts
     , configAmazon :: !Amazon
     } deriving Show

makeFields ''Config
makeFields ''Ports
makeFields ''Db
makeFields ''HasqlSettings
makeFields ''Ekg
makeFields ''Katip
makeFields ''Auth
makeFields ''Amazon

-- Load program configuration from file (server.yaml), or
-- raise YamlException and terminate program.
load :: FilePath -> IO Config
load path = decodeFileEither path >>= either throwIO pure

deriveFromJSON defaultOptions ''Db
deriveFromJSON defaultOptions ''Config
deriveFromJSON defaultOptions ''HasqlSettings
deriveFromJSON defaultOptions ''Ekg
deriveFromJSON defaultOptions ''Katip
deriveFromJSON defaultOptions ''Auth
deriveFromJSON defaultOptions ''ApiKeys
deriveFromJSON defaultOptions ''Service
deriveFromJSON defaultOptions ''Amazon