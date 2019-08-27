{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.FullText.Search (provider, qualification) where

import EdgeNode.Model.Provider
import EdgeNode.Model.Qualification
import qualified EdgeNode.Iso as Iso

import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.Acquire
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Database.FullText.Index
import qualified Database.Exception as Exception
import Katip
import Control.Lens.Iso.Extended
import Control.Lens
import Control.Monad
import Katip.Core (getLoc)
import Data.String.Interpolate

getResults 
  :: (KatipContext m,
      MonadPlus m1)
  => ([PersistValue] -> a) 
  -> QueryString 
  -> T.Text 
  -> TryAction 
     (Exception.Groundhog ()) 
     m Postgresql (m1 a)
getResults valuesToObj (QueryString query) term = 
  do 
    let term' = T.replace " " "<->" (T.strip term) <> ":*"
    rows <- queryRaw False query [PersistText term']
    $(logTM) InfoS [i|search: #{query}, #{term}|]
    values <- liftIO.withAcquire rows $ unfoldM'
    return $ fmap valuesToObj values

provider 
  :: (KatipContext m,
      MonadPlus m1) 
  => Maybe Language 
  -> T.Text 
  -> TryAction 
     (Exception.Groundhog ()) 
     m Postgresql (m1 (ProviderId, Provider))
provider lang = getResults valuesToObj (providerSearchQuery lang) 
  where valuesToObj
          [ ident
          , PersistText title
          , PersistText country] 
          = ( ProviderKey ident^.from autokey
            , Provider 
              (title^.from lazytext) 
              (country^.from Iso.country))
        valuesToObj x = error [i|#{show getLoc} : #{show x}|]

qualification         
  :: (KatipContext m,
      MonadPlus m1) 
  => Maybe Language 
  -> T.Text 
  -> TryAction 
  (Exception.Groundhog ()) 
  m Postgresql (m1 (QualificationId, QualificationFullInfo))
qualification lang = getResults valuesToObj (qualificationSearchQuery lang)
  where valuesToObj = undefined