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
import qualified Data.Text as T
import Orm.PersistField ()
import Data.Default.Class.Extended
import Data.Swagger
import Control.Lens
import Proto3.Suite.Types
import Data.Either
import Control.Lens.Iso.Extended
import Test.QuickCheck
import Database.Transaction (ParamsShow (..))

instance Default Gender where
  def = toEnum 0   
  
instance Default User
instance ToParamSchema UserId 

instance Arbitrary UserId where
  arbitrary = UserId <$> arbitrary

instance ParamsShow UserId where 
  render (UserId x) = render x 

mkToSchemaAndJSONProtoIdent ''UserId
makeWrapped ''UserId
mkFromHttpApiDataIdent ''UserId
mkEnumConvertor ''Gender

defUser :: User
defUser = def

coercedUserGender :: Enumerated Gender -> Gender
coercedUserGender x = x^.(coerced :: Iso' (Enumerated Gender) (Either Int Gender)).to (fromRight undefined)

hasqlEnumUserGender :: T.Text -> Maybe Gender
hasqlEnumUserGender = Just . toGender . (^.from stext)