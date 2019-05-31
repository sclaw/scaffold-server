{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Api (ApplicationApi (..), AuthApi (..), api) where


import           Servant.API.Generic
import           Servant.API.WebSocket
import           Data.Proxy
import           Servant.API

data ApplicationApi route = 
     ApplicationApi 
     { root :: route :- Get '[PlainText] String
     , auth :: route :- ToServant AuthApi AsApi 
     } deriving stock Generic

data AuthApi route = 
     AuthApi 
     { register 
       :: route
       :- "register" 
       :> WebSocket
     , authorize
       :: route
       :- "authorize" 
       :> WebSocket      
     , checkLoginAvailability
       :: route
       :- "check-login" 
       :> WebSocket  
     , checkEmailAvailability
       :: route
       :- "check-email" 
       :> WebSocket 
     } deriving stock Generic

api :: Proxy (ToServantApi ApplicationApi)
api = genericApi (Proxy :: Proxy ApplicationApi)

