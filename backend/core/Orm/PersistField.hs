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
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Orm.PersistField () where

import           Control.Lens
import qualified Data.Sequence as Seq
import           Database.Groundhog ()
import           Database.Groundhog.Core hiding (Utf8)
import           Database.Groundhog.Generic
import           Database.Groundhog.TH
import           Text.ProtocolBuffers.Basic (Utf8(..))
import           TH.Instance
import           Control.Lens.Iso.Extended


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

derivePrimitivePersistField ''Utf8 [| from tutf8 |]


