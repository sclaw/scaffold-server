{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module WebSocket () where

import           Network.WebSockets
import           Text.ProtocolBuffers.Reflections
import           Text.ProtocolBuffers.WireMessage
import           Control.Lens
import           Control.Lens.Iso.Extended

instance (Wire a, ReflectDescriptor a) => WebSocketsData a where
    fromDataMessage (Binary bs) = fromLazyByteString bs
    fromDataMessage _ = 
     error "Textual UTF-8 encoded data got instead of ArrayBuffer"
    fromLazyByteString  = (^.proto)
    toLazyByteString = (^.from proto)