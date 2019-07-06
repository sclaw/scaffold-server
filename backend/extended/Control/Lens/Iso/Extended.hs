{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Lens.Iso.Extended
       (
          textbs
        , textbsl
        , integral
        , stext
        , seql
        , proto
        , tutf8
        , sutf8
        , listSeq
        , stringify
       ) where

import           Control.Lens
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Foldable           (toList)
import qualified Data.Sequence           as Seq
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Text.ProtocolBuffers.Reflections
import           Text.ProtocolBuffers.WireMessage
import           Text.ProtocolBuffers.Basic
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.String

-- WARNING: Strictly speaking, 'utf8' is not isomorphism, since exists
-- ByteString, that is not decodable as Text. But it is very convenient.
textbs :: Iso' T.Text B.ByteString
textbs = iso T.encodeUtf8 T.decodeUtf8

textbsl :: Iso' T.Text BL.ByteString
textbsl = iso (LT.encodeUtf8 . LT.fromStrict)
                 (LT.toStrict . LT.decodeUtf8)

integral :: (Integral a, Integral b) => Iso' a b
integral = iso fromIntegral fromIntegral

stext :: Iso' String T.Text
stext = iso T.pack T.unpack

seql :: Iso' [a] (Seq.Seq a)
seql = iso Seq.fromList toList

proto :: (Wire a, ReflectDescriptor a) => Iso' BL.ByteString a
proto = iso (either (error . ("proto decode error: " ++)) fst . messageGet) messagePut

tutf8 :: Iso' T.Text Utf8
tutf8 = iso (view $ textbs.from strict.to Utf8) decode 
  where decode (Utf8 bs) = bs^.strict.from textbs

sutf8 :: Iso' String Utf8
sutf8 = iso (Utf8 . fromString) decode
  where decode (Utf8 bs) = C8.unpack bs

listSeq :: Iso' [a] (Seq.Seq a)
listSeq = iso Seq.fromList toList

stringify :: (Show a, Read a) => Iso' a String
stringify = iso show read 