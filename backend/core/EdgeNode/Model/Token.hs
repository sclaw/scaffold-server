{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module EdgeNode.Model.Token (Token, TokenConstructor (..), Field (..)) where

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.Time
import EdgeNode.Model.User (UserId)

data Token =
     Token
     {  tokenAccessToken  :: !String
      , tokenRefreshToken :: !String
      , tokenCreated      :: !UTCTime
      , tokenUserId       :: !UserId
     }

mkPersist_ [groundhog| 
 - entity: Token
   schema: auth
 |]