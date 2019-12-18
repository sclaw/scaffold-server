{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-- | Type for ids that is shared across all the projects.
-- user id is int64, so it's encrypted as "text" in json,
-- otherwise js code may fail to work with it.
newtype Id = Id Int64
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

instance ToSchema Id where
  declareNamedSchema _ = do
    srcSchema <- declareSchemaRef (Proxy :: Proxy String)
    pure $ NamedSchema (Just "Id") $ mempty
      & type_ .~ SwaggerString
      & properties .~ [("type", srcSchema)]
      & required .~ ["type"]

instance ToJSON Id where
  toJSON (Id v) = toJSON (T.pack (show v))

instance FromJSON Id where
  parseJSON (String t) = Id <$> case reads (T.unpack t) of
    [(x,_)] -> pure x
    _ -> fail "unable to parse"
  parseJSON (Number n) = 
    either 
    (const (fail "id can't be floating value")) 
    (pure . Id) 
    (floatingOrInteger @Double n)
  parseJSON _ = fail "id should be integral value or string"