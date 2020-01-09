{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Search (SearchApi (..)) where

import Servant.API.Generic
import Servant.API

data SearchApi route = 
     SearchApi
     { _searchApiSearch
       :: route
       :- Description ""
       :> Get '[JSON] () 
     } deriving stock Generic