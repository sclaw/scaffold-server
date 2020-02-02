{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Code from "Servant.Server.Internal", modified very slightly to allow for returning json
-- errors instead of plaintext.
module Servant.API.Extended 
       ( ReqBodyCustomError
       , MakeCustomError (..)
       , module Servant.API
       ) where

import Control.Monad.Trans (liftIO)
import Data.EitherR (fmapL)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Typeable
import Network.HTTP.Types hiding (Header, ResponseHeaders)
import Network.Wai
import Servant.API
import Servant.Swagger
import Servant.API.Modifiers
import Servant.API.ContentTypes
import Servant.Server.Internal
import GHC.TypeLits
import qualified Data.ByteString.Lazy as BL


-- | Like 'ReqBody'', but takes parsers that throw 'ServantErr', not 'String'.  @tag@ is used
-- to select a 'MakeCustomError' instance.
--
-- FUTUREWORK: this does not reflect the changes we make to the error responses wrt. the
-- 'ReqBody'' instance.  however, in order to fix that, we would need to get more information
-- out of the 'MakeCustomError' instance and into 'ReqBodyCustomError''. Perhaps something
-- like @data ReqBody (mods :: [*]) (headers :: ...) (status :: ...) (list :: [ct]) (tag ::
-- Symbol) (a :: *)@.  and then we'll trip over issues similar to this one:
-- https://github.com/wireapp/servant-uverb/blob/3647c488a88137d3ec2583b518bda59ee7072278/servant-uverb/src/Servant/API/UVerb.hs#L33-L57
--
-- FUTUREWORK: this approach is not ideal because it makes it hard to avoid orphan instances.
--
-- FUTUREWORK: parser failures currently can't have custom monad effects like logging, since
-- they are run inside 'DelayedIO'.  we can either work around this by writing a middleware
-- that inspects the response and logs conditionally what it finds in the body (bad for
-- streaming and performance!), or re-wire more of the servant internals (unclear how hard
-- that'll be).
--
-- See also: https://github.com/haskell-servant/servant/issues/353
data ReqBodyCustomError' (mods :: [*]) (list :: [ct]) (tag :: Symbol) (a :: *)

type ReqBodyCustomError = ReqBodyCustomError' '[Required, Strict]

-- | Custom parse error for bad request bodies.
class MakeCustomError (tag :: Symbol) (a :: *) where
  makeCustomError :: String -> ServantErr

-- | Variant of the 'ReqBody'' instance that takes a 'ServantErr' as argument instead of a
-- 'String'.  This gives the caller more control over error responses.
instance ( MakeCustomError tag a
         , AllCTUnrender list a
         , HasServer api context
         , SBoolI (FoldLenient mods)
         ) => HasServer (ReqBodyCustomError' mods list tag a :> api) context where

  type ServerT (ReqBodyCustomError' mods list tag a :> api) m =
    If (FoldLenient mods) (Either ServantErr a) a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver
      = route (Proxy :: Proxy api) context $
          addBodyCheck subserver ctCheck bodyCheck
    where
      -- Content-Type check, we only lookup we can try to parse the request body
      ctCheck = withRequest $ \ request -> do
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeH = fromMaybe "application/octet-stream"
                         $ lookup hContentType $ requestHeaders request
        case canHandleCTypeH (Proxy :: Proxy list) (cs contentTypeH) :: Maybe (BL.ByteString -> Either String a) of
          Nothing -> delayedFail err415
          Just f  -> return f

      -- Body check, we get a body parsing functions as the first argument.
      bodyCheck :: (BL.ByteString -> Either String a)
                -> DelayedIO (If (FoldLenient mods) (Either ServantErr a) a)
      bodyCheck f = withRequest $ \ request -> do
        mrqbody <- fmapL (makeCustomError @tag @a) . f <$> liftIO (lazyRequestBody request)
        case sbool :: SBool (FoldLenient mods) of
          STrue -> return mrqbody
          SFalse -> case mrqbody of
            Left e  -> delayedFailFatal e
            Right v -> return v

instance (HasSwagger (ReqBody' '[Required, Strict] cts a :> api)) => 
          HasSwagger (ReqBodyCustomError cts tag a :> api) where
    toSwagger Proxy = toSwagger (Proxy @(ReqBody' '[Required, Strict] cts a :> api))