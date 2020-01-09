{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Auth (AuthApi (..)) where

import Servant.API.Generic
import Servant.API.WebSocket ()
import Servant.API

data AuthApi route = 
     AuthApi
     { _authApiRegistration
       :: route
       :- Description "simple registration" 
       :> Post '[JSON] ()
     } deriving stock Generic