{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Lens.Iso.Extended
       (
          textbsiso
        , textbsliso
        , integraliso
        , stextiso
        , seqliso
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
textbsiso :: Iso' T.Text B.ByteString
textbsiso = iso T.encodeUtf8 T.decodeUtf8

textbsliso :: Iso' T.Text BL.ByteString
textbsliso = iso (LT.encodeUtf8 . LT.fromStrict)
                 (LT.toStrict . LT.decodeUtf8)

integraliso :: (Integral a, Integral b) => Iso' a b
integraliso = iso fromIntegral fromIntegral

stextiso :: Iso' String T.Text
stextiso = iso T.pack T.unpack

seqliso :: Iso' [a] (Seq.Seq a)
seqliso = iso Seq.fromList toList
