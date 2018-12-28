{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (AppApi, appApi) where

import           Servant

type AppApi = "getAllIntegers" :> Get '[JSON] [Int]

appApi :: Proxy AppApi
appApi = Proxy
