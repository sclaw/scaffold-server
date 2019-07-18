{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module Database.AutoKey (AutoKey'(..), deriveAutoKey) where
    
import Control.Lens
import Database.Groundhog.Core
import Language.Haskell.TH

-- AutoKey is defined in Database.Groundhog.Core
class AutoKey' p k | k -> p, p -> k where
  autokey :: Iso' p k

-- This function is quite opinionated about naming of related
-- types and constructor.
deriveAutoKey :: Name -> DecsQ
deriveAutoKey name = do
  let idN  = mkName $ nameBase name ++ "Id"
      keyN = mkName $ nameBase name ++ "Key"
  v <- newName "v"
  [d|
    instance AutoKey' $(conT idN) (Key $(conT name) BackendSpecific) where
      autokey = iso encode decode where
        encode $(conP idN [varP v]) = $(conE keyN) (PersistInt64 $(varE v))
        decode :: Key $(conT name) BackendSpecific -> $(conT idN)
        decode $(conP keyN [conP 'PersistInt64 [varP v]]) = $(conE idN) $(varE v)
    |]