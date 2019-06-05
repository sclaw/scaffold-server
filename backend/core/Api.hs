{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
     , auth :: route :- "auth" :> ToServant AuthApi AsApi 
     } deriving stock Generic

data AuthApi route = 
     AuthApi 
     { register 
       :: route
       :- "register" 
       :> WebSocketPending
     , authenticate
       :: route
       :- "authenticate" 
       :> WebSocketPending      
     , checkLoginAvailability
       :: route
       :- "check-login" 
       :> WebSocketPending  
     , checkEmailAvailability
       :: route
       :- "check-email" 
       :> WebSocketPending
     , login    
       :: route
       :- "login" 
       :> WebSocketPending
     } deriving stock Generic

api :: Proxy (ToServantApi ApplicationApi)
api = genericApi (Proxy :: Proxy ApplicationApi)