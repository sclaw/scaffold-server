{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Statement.Feedback (put) where

import EdgeNode.Transport.Feedback
import EdgeNode.Transport.Iso

import Database.Transaction
import TH.Mk
import qualified Data.Text.Lazy as LT
import Control.Lens
import Control.Lens.Iso.Extended
import Data.List
import qualified Hasql.Statement as HS
import Hasql.TH
import qualified Protobuf.Scalar as Protobuf
import Data.Generics.Product.Fields
import Data.Maybe

mkEncoder ''Feedback
mkArbitrary ''Feedback

deriving instance Enum Reason

instance ParamsShow Feedback where
  render feedback = intercalate ", " $
    ((mkEncoderFeedback feedback)
    & _1 %~ fromMaybe "null" . fmap (^.field @"uint64Value".integral.to show)
    & _2 %~ (^.lazytext.from stext)
    & _3 %~ (^.reason.from stext)
    & _4 %~ (^.lazytext.from stext)
    & _5 %~ (^.lazytext.from stext))^..each

put :: HS.Statement Feedback ()
put = lmap encoder $
  [resultlessStatement|
    insert into public.feedback
    (image_fk, who, reason, message, email)
    values ($1 :: int8?, $2 :: text, $3 :: text, $4 :: text, $5 :: text)|]
  where
    encoder x =
      (mkEncoderFeedback x)
      & _1 %~ fmap (^.field @"uint64Value".integral)
      & _2 %~ (^.lazytext)
      & _3 %~ (^.reason)
      & _4 %~ (^.lazytext)
      & _5 %~ (^.lazytext)