
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module EdgeNode.Statement.Auth 
       ( getUserCred
       , putRefreshToken
       ) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Id
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
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Int

mkEncoder ''SigninReq

instance ParamsShow SigninReq where
  render x = intercalate "," 
    [ fromMaybe mempty $ 
      x^?field @"signinReqProvider"._Just.field @"stringValue".from stextl
    , x^.field @"signinReqEmail".from stextl
    , x^.field @"signinReqPassword".from stextl
    ]

getUserCred :: HS.Statement SigninReq (Maybe (Id, Type, PassHash))
getUserCred = dimap (initT . mkTpl . mkEncoderSigninReq) decoder statement
  where
    mkTpl x = 
      x & _1 %~ maybe mempty (^.field @"stringValue") 
        & each %~ (^.lazytext)
    statement = 
      [maybeStatement|
        select 
          id :: int8,
          "user_type" :: text,
          password :: bytea
        from auth.user 
        where identifier = md5($1 :: text || $2 :: text)|]
    decoder x =  
      x  & _Just._1 %~ coerce 
         & _Just._2 %~ (^.from stext.from isoType) 
         & _Just._3 %~ (^.from textbs.to coerce)

instance ParamsShow (B.ByteString, Id, T.Text) where 
    render x = intercalate "," 
      [ x^._1.from textbs.from stext
      , x^._2.coerced @_ @_ @Int64 @_.to show
      , x^._3.from stext] 
    
putRefreshToken :: HS.Statement (B.ByteString, Id, T.Text) Bool
putRefreshToken = dimap (& _2 %~ coerce) (> 0) statement
  where
    statement =
      [rowsAffectedStatement|
        insert into auth.token 
        (token, created, user_fk, uid) 
        values ($1 :: bytea, now(), $2 :: int8, $3 :: text)
        on conflict do nothing|] 