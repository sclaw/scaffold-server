
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module EdgeNode.Statement.Auth
       ( -- * statements
         getUserCred
       , putRefreshToken
       , checkRefreshToken
       , mkTokenInvalid
       , logout
       , register
       , putResetPasswordToken
       , getTokenStatus
       , setNewPassword
       , getTokenUsageWithPass
       , getUserId
       ) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Id
import EdgeNode.Model.User
import EdgeNode.Statement.Provider ()
import EdgeNode.Transport.User
import EdgeNode.Model.Auth

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
import Data.Time.Clock
import Data.String.Conv

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

getUserCred :: HS.Statement Signin (Maybe (Id "user", T.Text, UserRole, PassHash))
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

instance ParamsShow (B.ByteString, Id "user", T.Text) where
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

checkRefreshToken :: HS.Statement (T.Text, Id "user") (Maybe (Int64, T.Text, UserRole))
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

logout :: HS.Statement (Id "user", OnlyField "hash" T.Text) (Either () Unit)
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

putResetPasswordToken :: HS.Statement (Id "user", TokenType, B.ByteString, UTCTime) (Maybe T.Text)
putResetPasswordToken =
  lmap (consT (New^.isoTokenProcessStatus.stext) . (\x -> x & _1 %~ (coerce @_ @Int64) & _2 %~ (^.isoTokenType.stext)))
  [singletonStatement|
    with new_updater as (
      insert into auth.password_updater
      (token, valid_until)
      values ($4 :: bytea, $5 :: timestamptz)
      returning id),
    blocks as (
      select block_until as until
      from auth.user_password_updater
      where "user_fk" = $2 :: int8
      and token_type = $3 :: text),
    new_user as (
      insert into auth.user_password_updater
      ("user_fk", password_updater_fk, status, token_type)
      select $2 :: int8, id, $1 :: text, $3 :: text
      from new_updater
      on conflict ("user_fk", token_type)
      do update set
        attempts =
        attempts =
          case when (select until from blocks) is not null
               and (select until from blocks) < now() then 1
          when (select until from blocks) is not null
               and (select until from blocks) > now()
          then (select att from blocks)
          else (select att from blocks) + 1 end,
        block_until =
          case when (select att from blocks)  > 2
               and (select until from blocks) is null
          then now() + interval '10 sec'
          when (select att from blocks)  > 2
               and (select until from blocks) is not null
               and (select until from blocks) > now()
          then (select until from blocks)
          else null end
      returning "user_fk")
    select (u.name || ' ' || u.surname) :: text?
    from new_user as nu
    inner join edgenode.user as u
    on nu."user_fk" = u.user_id
    union
    select null :: text?
    from new_user as nu
    inner join edgenode.provider_user as pu
    on nu."user_fk" = pu.user_id|]

getTokenStatus :: HS.Statement (Id "user", TokenType) (Maybe (Maybe UTCTime))
getTokenStatus =
  lmap (\x -> x & _1 %~ (coerce @_ @Int64) & _2 %~ (^.isoTokenType.stext))
  [maybeStatement|
    select upu.block_until :: timestamptz?
    from auth.user_password_updater as upu
    where upu.user_fk = $1 :: int8
    and upu.token_type = $2 :: text|]

setNewPassword :: HS.Statement (Id "user", TokenType, B.ByteString) ()
setNewPassword =
  lmap (consT (Completed^.isoTokenProcessStatus.stext) . (\x -> x & _1 %~ (coerce @_ @Int64) & _2 %~ (^.isoTokenType.stext)))
  [resultlessStatement|
    with new_user as (
      update auth.user set
      password = $4 :: bytea,
      password_modified_tm = now()
      where id = $2 :: int8),
    pass as (
      update auth.user_password_updater set
      status = $1 :: text,
      block_until = now() + interval '30 day',
      attempts = 0
      where "user_fk" = $2 :: int8
      and token_type = $3 :: text
      returning password_updater_fk as ident)
    update auth.password_updater set done_tm = now()
    where id = any(select * from pass)|]

getTokenUsageWithPass :: HS.Statement (Id "user", TokenType) (Maybe (Bool, B.ByteString))
getTokenUsageWithPass =
  lmap (\x -> x & _1 %~ (coerce @_ @Int64) & _2 %~ (^.isoTokenType.stext))
  [maybeStatement|
    select done_tm is null :: bool, u.password :: bytea
    from auth.user_password_updater as upu
    inner join auth.password_updater as pu
    on upu.password_updater_fk = pu.id
    inner join auth.user as u
    on upu.user_fk = u.id
    where upu.user_fk = $1 :: int8
    and upu.token_type = $2 :: text|]

instance ParamsShow UserRole where render = fromUserRole

getUserId :: HS.Statement (UserRole, T.Text) (Maybe (Id "user"))
getUserId =
  dimap (& _1 %~ (toS . fromUserRole)) (fmap coerce)
  [maybeStatement|select id :: int8 from auth.user where "user_type" = $1 :: text and email = $2 :: text|]