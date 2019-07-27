{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Api 
       ( HttpApi (..)
       , module EdgeNode.Api.Http.Auth
       , module EdgeNode.Api.Http.User
       ) where

import EdgeNode.Api.Http.Auth
import EdgeNode.Api.Http.User

import Servant.API.Generic
import Servant.API.WebSocket ()
import Servant.API
import Servant.Auth.Swagger ()
import Swagger.ToSchema ()


data HttpApi route = 
     HttpApi 
     { httpApiAuth
       :: route 
       :- "auth" 
       :> ToServant AuthApi AsApi
     , httpApiUser
       :: route
       :- "user"
       :> ToServant UserApi AsApi            
     } deriving stock Generic 
