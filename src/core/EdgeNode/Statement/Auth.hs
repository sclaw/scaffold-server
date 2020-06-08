
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
       , putResetPasswordToken
       ) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Id
import EdgeNode.Model.User
import EdgeNode.Statement.Provider ()
import EdgeNode.Transport.User

import Auth
import qualified Hasql.Statement as HS
import TH.Mk
import TH.Proto
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
import Test.QuickCheck (Arbitrary (..))

mkEncoder ''Signin
mkEncoder ''Registration
mkArbitrary ''Signin
mkArbitrary ''Registration

instance Arbitrary Salt where arbitrary = Salt <$> arbitrary
instance ParamsShow Salt where render = render . coerce @Salt @B.ByteString

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

getUserCred :: HS.Statement Signin (Maybe (UserId, T.Text, UserRole, PassHash))
getUserCred = dimap (initT . mkTpl . mkEncoderSignin) decoder statement
  where
    mkTpl x =
      x & _1 %~ maybe mempty (^.field @"stringValue")
        & each %~ (^.lazytext)
    statement =
      [maybeStatement|
        select
          id :: int8,
           email :: text,
          "user_type" :: text,
          password :: bytea
        from auth.user
        where identifier = md5($2 :: text || $1 :: text)|]
    decoder x =
      x  & _Just._1 %~ coerce
         & _Just._3 %~ (^.from stext.from isoUserRole)
         & _Just._4 %~ (^.from textbs.to coerce)

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

checkRefreshToken :: HS.Statement (T.Text, UserId) (Maybe (Int64, T.Text, UserRole))
checkRefreshToken = dimap (& _2 %~ coerce) (fmap (& _3 %~ (^.from stext.from isoUserRole))) $
  [maybeStatement|
    select
      at.id :: int8,
      email ::text,
      "user_type" :: text
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

register :: HS.Statement (Salt, Registration) (Maybe Int64)
register = lmap mkEncoder statement
  where
    mkEncoder (salt, x) =
      consT (Primary^.isoUserRole.stext) $
      consT (Active^.isoRegisterStatus.stext) $
      consT (GenderMale^.isoGender.stext) $
      (initT (mkEncoderRegistration x)
      & _1 %~ (^.lazytext)
      & _2 %~
        (^.lazytext
         .to (unPassHash . hashPassWithSalt salt . mkPass)
         .textbs))
    statement =
      [maybeStatement|
        with
         get_user as
          (insert into auth.user
           (identifier, password, user_type, email)
           values (md5($4 :: text), $5 :: bytea, $1 :: text, $4 :: text)
           on conflict do nothing
           returning id),
         get_day as
          (insert into public.full_day
           (year, month, day)
           values (0, 0, 0)
           returning id)
        insert into edgenode.user
        (user_id, status, birthday_id, gender)
        (select id, $2 :: text, (select id from get_day), $3 :: text from get_user)
        returning (select id from get_user) :: int8|]

putResetPasswordToken :: HS.Statement (UserId, B.ByteString) (Maybe (Maybe T.Text))
putResetPasswordToken =
  lmap (& _1 %~ coerce)
  [maybeStatement|
    with
      next as (
        select count(*) as i
        from auth.password_updater
        where "user_fk" = $1 :: int8
        and status = 'completed'),
      token as (
      insert into auth.password_updater
      ("user_fk", serial, status, token, attempts)
      values (
        $1 :: int8,
        (select (i + 1) from next),
        'new',
        $2 :: bytea,
        1)
      on conflict ("user_fk", serial)
      do update set attempts = excluded.attempts + 1
      returning
        case when attempts = 3
             then null
             else "user_fk"
        end as ident)
    select (u.name || ' ' || u.surname) :: text?
    from token as t
    inner join edgenode.user as u
    on t.ident = u.user_id
    union
    select null :: text?
    from token as t
    inner join edgenode.provider_user as pu
    on t.ident = pu.user_id|]