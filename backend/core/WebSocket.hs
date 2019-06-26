{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module WebSocket (Data (..)) where

import           Network.WebSockets
import           Text.ProtocolBuffers.Reflections
import           Text.ProtocolBuffers.WireMessage
import           Control.Lens
import           Control.Lens.Iso.Extended
import qualified Data.ByteString.Lazy as B

newtype Data a = Data { unwrapData :: Either B.ByteString a } 

instance (Wire a, ReflectDescriptor a) => WebSocketsData (Data a) where
    fromDataMessage (Binary bs) = fromLazyByteString bs
    fromDataMessage (Text bs _) = Data $ Left bs
    fromLazyByteString bs = Data $ Right (bs^.proto)
    toLazyByteString (Data (Right x)) = x^.from proto
    toLazyByteString (Data (Left bs)) = bs