
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
       , mkTokenInvalid
       , logout
       , register
       ) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Id
import EdgeNode.Model.User
import EdgeNode.Statement.Provider ()

import Auth
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
import Data.Password
import Data.Coerce
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Int
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Tuple.Extended

mkEncoder ''Signin
mkEncoder ''Registration
mkArbitrary ''Signin 
mkArbitrary ''Registration

instance ParamsShow Signin where
  render x = intercalate "," 
    [ fromMaybe mempty $ 
      x^?field @"signinProvider"._Just.field @"stringValue".from stextl
    , x^.field @"signinEmail".from stextl
    , x^.field @"signinPassword".from stextl]

instance ParamsShow Registration where 
  render x = intercalate "," 
    [ x^.field @"registrationEmail".from stextl
    , x^.field @"registrationPassword".from stextl]

getUserCred :: HS.Statement Signin (Maybe (UserId, UserRole, PassHash))
getUserCred = dimap (initT . mkTpl . mkEncoderSignin) decoder statement
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
        where identifier = md5($2 :: text || $1 :: text)|]
    decoder x =  
      x  & _Just._1 %~ coerce 
         & _Just._2 %~ (^.from stext.from isoUserRole) 
         & _Just._3 %~ (^.from textbs.to coerce)

instance ParamsShow (B.ByteString, UserId, T.Text) where 
    render x = intercalate "," 
      [ x^._1.from textbs.from stext
      , x^._2.coerced @_ @_ @Int64 @_.to show
      , x^._3.from stext] 
    
putRefreshToken :: HS.Statement (UserId, T.Text, T.Text) ()
putRefreshToken = lmap (& _1 %~ coerce) statement
  where
    statement =
      [resultlessStatement|
        insert into auth.token 
        (created, user_fk, refresh_token_hash, "unique") 
        values (now(), $1 :: int8, $2 :: text, $3 :: text)|] 

checkRefreshToken :: HS.Statement (T.Text, UserId) (Maybe (Int64, UserRole))
checkRefreshToken = dimap (& _2 %~ coerce) (fmap (& _2 %~ (^.from stext.from isoUserRole))) $
  [maybeStatement|
    select at.id :: int8, "user_type" :: text
    from auth.token as at
    inner join auth.user as au
    on at."user_fk" = au.id
    where "unique" = $1 :: text    
    and "user_fk" = $2 :: int8 
    and is_valid|]

mkTokenInvalid :: HS.Statement Int64 ()
mkTokenInvalid = [resultlessStatement|update auth.token set is_valid = false where id = $1 :: int8|]

logout :: HS.Statement (UserId, OnlyField "hash" T.Text) (Either () Unit)
logout = dimap (bimap coerce coerce) decoder statement
  where 
    decoder i | i > 0 = Right Unit
              | otherwise = Left mempty
    statement = 
      [rowsAffectedStatement|
        delete from auth.token 
        where "user_fk" = $1 :: int8 and 
        refresh_token_hash = $2 :: text|]

register :: Salt -> HS.Statement Registration Bool
register salt = dimap mkEncoder (> 0) statement
  where 
    mkEncoder x = 
      consT (Primary^.isoUserRole.stext) $
      consT (Active^.isoRegisterStatus.stext) $
      (initT (mkEncoderRegistration x) 
      & _1 %~ (^.lazytext) 
      & _2 %~ 
        (^.lazytext
         .to (unPassHash . hashPassWithSalt salt . mkPass)
         .textbs)) 
    statement = 
      [rowsAffectedStatement|
        with 
         get_user as 
          (insert into auth.user
           (identifier, password, user_type)
           values (md5($3 :: text), $4 :: bytea, $1 :: text)
           on conflict do nothing
           returning id),
         get_day as 
          (insert into public.full_day
           (year, month, day) 
           values (0, 0, 0) 
           returning id)   
        insert into edgenode.user 
        (user_id, status, birthday_id) 
        (select id, $2 :: text, (select id from get_day) from get_user)|]