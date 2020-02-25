
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
       , checkRefreshToken
       ) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Id
import EdgeNode.Model.User
import EdgeNode.Statement.Provider ()

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
import Test.QuickCheck.Extended
import Test.QuickCheck.Arbitrary.Generic

mkEncoder ''SigninReq

instance Arbitrary SigninReq where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance ParamsShow SigninReq where
  render x = intercalate "," 
    [ fromMaybe mempty $ 
      x^?field @"signinReqProvider"._Just.field @"stringValue".from stextl
    , x^.field @"signinReqEmail".from stextl
    , x^.field @"signinReqPassword".from stextl
    ]

getUserCred :: HS.Statement SigninReq (Maybe (Id "user", UserRole, PassHash))
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
         & _Just._2 %~ (^.from stext.from isoUserRole) 
         & _Just._3 %~ (^.from textbs.to coerce)

instance ParamsShow (B.ByteString, Id "user" , T.Text) where 
    render x = intercalate "," 
      [ x^._1.from textbs.from stext
      , x^._2.coerced @_ @_ @Int64 @_.to show
      , x^._3.from stext] 
    
putRefreshToken :: HS.Statement (Id "user", T.Text, T.Text) ()
putRefreshToken = lmap (& _1 %~ coerce) statement
  where
    statement =
      [resultlessStatement|
        insert into auth.token 
        (created, user_fk, refresh_token_hash, "unique") 
        values (now(), $1 :: int8, $2 :: text, $3 :: text)|] 

checkRefreshToken :: HS.Statement (T.Text, Id "user") (Maybe UserRole)
checkRefreshToken = dimap (& _2 %~ coerce) (fmap (^.from stext.from isoUserRole)) $
  [maybeStatement|
    select "user_type" :: text
    from auth.token as at
    inner join auth.user as au
    on at."user_fk" = au.id
    where "unique" = $1 :: text and "user_fk" = $2 :: int8|]