{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Http.GetProvider (controller) where

import EdgeNode.Api.Http.User.GetProvider
import EdgeNode.Error
import EdgeNode.Lang
import EdgeNode.Model.Provider

import Database.FullText.Search
import RetrofitProto
import KatipController
import ReliefJsonData
import Data.Generics.Product
import Control.Lens
import Database.Action
import Katip
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Database.Exception
import Control.Lens.Iso.Extended
import qualified Data.Text as T
import Data.Bifunctor
import Control.Monad.Error.Class 
       ( throwError
       , catchError)
import Control.Exception (fromException)
import Data.Vector.Lens

controller :: GetProviderRequest -> KatipController (Alternative (Error T.Text) GetProviderResponse)
controller req =
  do
    let _ = req^._Wrapped'.field @"requestIdent"
    let Just lang = req^?_Wrapped'.field @"requestLang".field @"enumerated"._Right
    let query = req^._Wrapped'.field @"requestQuery".lazytext
    orm <- fmap (^.katipEnv.ormDB) ask
    let logErr e = 
          do $(logTM) ErrorS (logStr (show e))
             throwError e    
    let mkError e = 
         maybe 
         (ServerError (InternalServerError (show e^.stextl))) 
         (const (ResponseError "unkonwn server error, please pay a visit to log")) 
         (fromException e :: Maybe Groundhog)
    (^.eitherToAlt) . first mkError <$> 
     runTryDbConnGH (action query lang `catchError` logErr) orm
    
action :: T.Text -> Language -> TryAction Groundhog KatipController Postgresql GetProviderResponse
action term lang = 
  do 
    xs :: [(ProviderId, Provider)] <- provider lang term
    let xs' = map (\(i, v) -> XProvider (Just i) (Just v)) $ 
              xs^..folded.filtered 
              ( (== fromLanguage lang) 
              . (^._2.field @"providerCountry".lazytext.from stext))
    return $ GetProviderResponse $ Response $ xs'^.vector