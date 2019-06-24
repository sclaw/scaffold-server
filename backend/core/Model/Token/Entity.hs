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
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Model.Token.Entity (Token, TokenConstructor (..), Field (..)) where

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.Time
import Model.User.Entity (UserId)

data Token =
     Token
     {  tokenAccessToken  :: !String
      , tokenRefreshToken :: !String
      , tokenCreated      :: !UTCTime
      , tokenUserId       :: !UserId
     }

mkPersist_ [groundhog| 
 - entity: Token
   schema: main  
 |]