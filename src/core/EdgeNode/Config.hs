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
       , Swagger (..)
       , db
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
       , userId
       , accessKey
       , secretKey
       , minio
       , bucketPrefix
       , swagger
       ) 
       where

import Data.Aeson    
import Data.Aeson.TH.Extended
import Control.Lens
import Control.Exception
import Data.Yaml
import Data.Time.Clock
import Data.Int
import qualified Data.Text as T

data Db = Db 
     { dbHost :: !String
     , dbPort :: !Int
     , dbUser :: !String
     , dbPass :: !String
     , dbDatabase :: !String
     } deriving Show

data Swagger = Swagger { swaggerHost :: String, swaggerPort :: Int }
  deriving Show

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

data Minio = 
     Minio
     { minioAccessKey :: !T.Text
     , minioSecretKey :: !T.Text
     , minioBucketPrefix :: !T.Text
     , minioHost :: !String
     , minioPort :: !String
     }
  deriving Show

data Config = 
     Config 
     { configDb :: !Db 
     , configSwagger :: !Swagger
     , configHasql :: !HasqlSettings
     , configKatip :: !Katip
     , configAuth :: !Auth
     , configService :: !Service
     , configMinio :: !Minio
     } deriving Show

makeFields ''Config
makeFields ''Db
makeFields ''HasqlSettings
makeFields ''Ekg
makeFields ''Katip
makeFields ''Auth
makeFields ''Minio
makeFields ''Swagger

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
deriveFromJSON defaultOptions ''Minio
deriveFromJSON defaultOptions ''Swagger