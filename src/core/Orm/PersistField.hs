{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Orm.PersistField () where

import EdgeNode.Country

import Time.Time  
import Control.Lens
import qualified Data.Sequence as Seq
import Database.Groundhog ()
import Database.Groundhog.Core
import Database.Groundhog.TH
import qualified Protobuf.Scalar
import TH.Mk (mkWrappedPrimitivePersistField, mkPrimitivePersistField)
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import GHC.Float
import Data.Aeson
import RetrofitProto
import ReliefJsonData
import Data.Bifunctor
import qualified Data.Aeson as Aeson
import Control.Lens.Iso.Extended

{- We need (PersistField (Seq a)) to make model out of protobuffer
 - datatypes. Unfortunatelly, Seq is a newtype, and Groundhog somewhy
 - refuses to generate PersistField instances for newtypes.
 -
 - Using internal module Data.Sequence.Internal, it is possible to
 - generate PersistField instance for underlying FingerTree, and write
 - boilerplate instance for Seq. But it hangs migration, for some
 - reason.
 -
 - Here is XList ~ [] ~ Seq. It seems to work. As I already mentioned,
 - Groundhog do not work with newtypes, so XList is data.
 -}
data XList a = XList ![a]
mkPersist defaultCodegenConfig [groundhog| 
 - entity: XList
 |]

instance (PersistField a) => PersistField (Seq.Seq a) where
  persistName _ = "containers:Data.Sequence.Seq"
  toPersistValues s = toPersistValues.XList $ s^..traverse
  fromPersistValues values = do
    (XList l, values') <- fromPersistValues values
    pure (Seq.fromList l, values')
  dbType p s = dbType p (XList $ s^..traverse)
  
instance PersistField ValueWrapper where 
  persistName _ = "JsonValueWrapper"
  toPersistValues (ValueWrapper x) = primToPersistValue x
  fromPersistValues = fmap (first ValueWrapper) . primFromPersistValue
  dbType _ _ = DbTypePrimitive DbBlob False Nothing Nothing

instance PrimitivePersistField ValueWrapper where
  toPrimitivePersistValue (ValueWrapper x) = PersistText $ Aeson.encode x^.from textbsl
  fromPrimitivePersistValue = ValueWrapper . fromPrimitivePersistValue

instance NeverNull ValueWrapper

instance PrimitivePersistField (Maybe ValueWrapper) where
  toPrimitivePersistValue (Just (ValueWrapper x)) = 
    PersistText $ Aeson.encode x^.from textbsl
  toPrimitivePersistValue _ = PersistNull
  fromPrimitivePersistValue PersistNull = Nothing  
  fromPrimitivePersistValue x = 
    (Just . ValueWrapper . fromPrimitivePersistValue) x

mkPrimitivePersistField ''Float [| iso float2Double double2Float |]

mkWrappedPrimitivePersistField ''Protobuf.Scalar.String
mkWrappedPrimitivePersistField ''Protobuf.Scalar.Bytes
mkWrappedPrimitivePersistField ''Protobuf.Scalar.Double
mkWrappedPrimitivePersistField ''Protobuf.Scalar.Float
mkWrappedPrimitivePersistField ''Protobuf.Scalar.Int32
mkWrappedPrimitivePersistField ''Protobuf.Scalar.Int64
mkWrappedPrimitivePersistField ''Protobuf.Scalar.UInt64
mkWrappedPrimitivePersistField ''Protobuf.Scalar.UInt32
mkWrappedPrimitivePersistField ''Protobuf.Scalar.SInt64
mkWrappedPrimitivePersistField ''Protobuf.Scalar.SInt32
mkWrappedPrimitivePersistField ''Protobuf.Scalar.Fixed64
mkWrappedPrimitivePersistField ''Protobuf.Scalar.Fixed32

mkPrimitivePersistField ''Country [| iso fromCountry toCountry |]

mkPrimitivePersistField ''FullDay [| iso toJSON (getVal . fromJSON) |]

getVal :: Result FullDay -> FullDay
getVal (Success x) = x
getVal e = error (show e)