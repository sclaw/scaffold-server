{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Site (SiteApi (..), NewsApi (..)) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Site

import Servant.API.Generic
import Servant.API.Extended

newtype SiteApi route =
        SiteApi
        { _siteApiNews
         :: route
         :- Description "landing page"
         :> "news"
         :> ToServant NewsApi AsApi
        } deriving stock Generic

data NewsApi route =
     NewsApi
     { _newsApiGetNewsFeed
       :: route
       :- Description ""
       :> "feed"
       :> Get '[JSON] (Response NewsFeed)
     , _newsApiGetNewsItem
       :: route
       :- Description ""
       :> "item"
       :> Capture "news_id" (Id "news")
       :> Get '[JSON] (Response News)
     } deriving stock Generic