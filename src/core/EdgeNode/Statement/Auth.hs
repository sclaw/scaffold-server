
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module  EdgeNode.Statement.Auth (getUserCred) where

import EdgeNode.Transport.Auth
import EdgeNode.Model.User

import qualified Hasql.Statement as HS
import TH.Mk
import qualified Data.Text.Lazy as LT
import qualified Protobuf.Scalar as Protobuf
import Database.Transaction
import Control.Lens
import Control.Lens.Iso.Extended
import Data.Generics.Product.Fields
import Data.List
import Data.Maybe
import Hasql.TH
import Data.Tuple.Ops
import Data.Password
import Data.Coerce

mkEncoder ''SigninReq

instance ParamsShow SigninReq where
  render x = intercalate "," 
    [ fromMaybe mempty $ 
      x^?field @"signinReqProvider"._Just.field @"stringValue".from stextl
    , x^.field @"signinReqEmail".from stextl
    , x^.field @"signinReqPassword".from stextl
    ]

getUserCred :: HS.Statement SigninReq (Maybe (Type, PassHash))
getUserCred = dimap (initT . mkTpl . mkEncoderSigninReq) (\x -> x & _Just._1 %~ (^.from stext.from isoType) & _Just._2 %~ (^.from textbs.to coerce)) statement
  where
    mkTpl x = 
      x & _1 %~ maybe mempty (^.field @"stringValue") 
        & each %~ (^.lazytext)
    statement = 
      [maybeStatement|
        select 
          "user_type" :: text,
          password :: bytea
        from auth.user 
        where identifier = md5($1 :: text || $2 :: text)|]