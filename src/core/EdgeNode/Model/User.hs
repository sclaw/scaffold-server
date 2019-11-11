{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Model.User  
       ( User (..)
       , UserId (..)
       , defUser
       , isoGender
       , fromGender
       , toGender
       , coercedUserGender
       , hasqlEnumUserGender
       ) where

import EdgeNode.User

import TH.Mk
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import qualified Data.Text as T
import Orm.PersistField ()
import Data.Default.Class.Extended
import Data.Swagger
import Control.Lens
import Proto3.Suite.Types
import Data.Either
import Control.Lens.Iso.Extended
import Data.Aeson
import Orphan ()

instance Default Gender where
  def = toEnum 0   
  
instance Default User
instance ToParamSchema UserId 

mkToSchemaAndJSONProtoIdent ''UserId
mkWrappedPrimitivePersistField ''UserId
mkFromHttpApiDataIdent ''UserId
mkEnumConvertor ''Gender
mkPrimitivePersistField ''Gender [| iso fromGender toGender |] 
mkPrimitivePersistFieldParam ''Enumerated [| enumGender |]

enumGender :: (FromJSON a, ToJSON a) => Iso' a T.Text
enumGender = 
  iso ((^.from textbsl.to strip._Just) . encode) 
      (either err id `fmap` 
       (eitherDecode . (^.to mkJson.textbsl))) 
  where err = error . (<>) "decode error: "
        mkJson x = "\"" <> x <> "\""
        strip x = do 
         x' <- T.stripPrefix "\"" x
         T.stripSuffix "\"" x'  

defUser :: User
defUser = def

coercedUserGender :: Enumerated Gender -> Gender
coercedUserGender x = x^.(coerced :: Iso' (Enumerated Gender) (Either Int Gender)).to (fromRight undefined)

hasqlEnumUserGender :: T.Text -> Maybe Gender
hasqlEnumUserGender = Just . toGender . (^.from stext)