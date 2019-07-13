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

module EdgeNode.Model.Token.Entity (Token, TokenConstructor (..), Field (..)) where

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.Time
import EdgeNode.Model.User.Entity (UserIdWrapper)

data Token =
     Token
     {  tokenAccessToken  :: !String
      , tokenRefreshToken :: !String
      , tokenCreated      :: !UTCTime
      , tokenUserId       :: !UserIdWrapper
     }

mkPersist_ [groundhog| 
 - entity: Token
   schema: main  
 |]