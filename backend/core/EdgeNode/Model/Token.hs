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

import EdgeNode.Model.User (UserId)

import Database.Groundhog.TH.Extended
import Database.Groundhog.Core (Field (..))
import Data.Time
import qualified Data.ByteString as B
import Data.Word (Word64)

data Token =
     Token
     { tokenRefreshToken :: !B.ByteString
     , tokenCreated      :: !UTCTime
     , tokenUserId       :: !UserId
     , tokenUnique       :: !Word64
     }

mkPersist_ [groundhog| 
 - entity: Token
   schema: auth
   constructors:
    - name: Token
      uniques: 
       - name: token_tokenUserId_tokenUnique_uk
         type: constraint
         fields: [tokenUserId, tokenUnique]   
 |]