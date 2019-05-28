{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Api (ApplicationApi (..), UserApi (..), api) where


import           Servant.API.Generic
import           Servant.API.WebSocket
import           Data.Proxy
import           Servant.API

data ApplicationApi route = 
     ApplicationApi 
     { root :: route :- Get '[PlainText] String
     , home :: route :- ToServant UserApi AsApi 
     } deriving stock Generic

data UserApi route = 
     UserApi 
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

