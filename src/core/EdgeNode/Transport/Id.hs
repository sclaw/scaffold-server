{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Transport.Id (Id(..)) where

import Data.Int
import Control.Lens
import Data.Aeson
import Data.Binary
import Data.Hashable
import Data.Scientific
import Data.Swagger
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import Servant.API
import Test.QuickCheck    (Arbitrary)
import TextShow
import Database.Transaction (ParamsShow (..))
import GHC.Types
import Data.Default.Class

-- | Type for ids that is shared across all the projects.
-- user id is int64, so it's encrypted as "text" in json,
-- otherwise js code may fail to work with it.
newtype Id (a :: Symbol) = Id Int64
  deriving newtype (Show, Eq)
  deriving newtype Arbitrary
  deriving newtype Binary
  deriving newtype Hashable
  deriving newtype Read
  deriving newtype Num
  deriving newtype Enum
  deriving newtype Real
  deriving newtype Integral
  deriving newtype TextShow
  deriving stock Generic
  deriving stock Ord
  deriving anyclass ToParamSchema
  deriving newtype FromHttpApiData
  deriving newtype ParamsShow

instance ToSchema (Id a) where
  declareNamedSchema _ = do
    srcSchema <- declareSchemaRef (Proxy :: Proxy String)
   --  let unique = T.pack $ show (typeOf (undefined :: a))
    pure $ NamedSchema (Just ("Id(" <> T.pack (show (Proxy @a)) <> ")")) $ mempty
      & type_ .~ SwaggerString
      & properties .~ [("id", srcSchema)]
      & required .~ ["id"]

instance ToJSON (Id a) where
  toJSON (Id v) = toJSON (T.pack (show v))

instance FromJSON (Id a) where
  parseJSON (String t) = Id <$> case reads (T.unpack t) of
    [(x,_)] -> pure x
    _ -> fail "unable to parse"
  parseJSON (Number n) = 
    either 
    (const (fail "id can't be floating value")) 
    (pure . Id) 
    (floatingOrInteger @Double n)
  parseJSON _ = fail "id should be integral value or string"

instance Default (Id a)