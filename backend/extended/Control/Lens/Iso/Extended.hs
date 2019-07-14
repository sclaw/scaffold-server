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
        , stextl
        , seql
        , listSeq
        , stringify
        , lazytext
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

stextl :: Iso' String LT.Text
stextl = iso LT.pack LT.unpack

lazytext :: Iso' LT.Text T.Text
lazytext = iso LT.toStrict LT.fromStrict

seql :: Iso' [a] (Seq.Seq a)
seql = iso Seq.fromList toList

listSeq :: Iso' [a] (Seq.Seq a)
listSeq = iso Seq.fromList toList

stringify :: (Show a, Read a) => Iso' a String
stringify = iso show read 