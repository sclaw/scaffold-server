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

import Time.Time  
import Control.Lens
import qualified Data.Sequence as Seq
import Database.Groundhog ()
import Database.Groundhog.Core
import Database.Groundhog.TH
import qualified Protobuf.Scalar
import TH.Generator (deriveWrappedPrimitivePersistField, derivePrimitivePersistField)
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import GHC.Float
import Data.Aeson

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

derivePrimitivePersistField ''Float [| iso float2Double double2Float |]

deriveWrappedPrimitivePersistField ''Protobuf.Scalar.String
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.Bytes
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.Double
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.Float
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.Int32
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.Int64
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.UInt64
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.UInt32
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.SInt64
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.SInt32
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.Fixed64
deriveWrappedPrimitivePersistField ''Protobuf.Scalar.Fixed32

derivePrimitivePersistField ''FullDay [| iso toJSON (getVal . fromJSON) |]

getVal :: Result FullDay -> FullDay
getVal (Success x) = x
getVal e = error (show e)