{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TransformListComp      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DerivingStrategies     #-}

module EdgeNode.Model.Auth
       ( TokenProcessStatus (..)
       , TokenType (..)
       , isoTokenProcessStatus
       , isoTokenType
       ) where

import TH.Mk
import Control.Lens
import Database.Transaction
import GHC.Generics
import Data.Aeson
import Data.String.Conv

data TokenProcessStatus = New | Aborted | Completed

data TokenType = ForgotPassword | NewPassword deriving Generic

mkEnumConvertor ''TokenProcessStatus
mkEnumConvertor ''TokenType
mkArbitrary ''TokenType

instance ParamsShow TokenType where render = (^.isoTokenType)
instance ToJSON TokenType where toJSON = String . toS . fromTokenType
instance FromJSON TokenType where parseJSON = withText "TokenType" (pure . toTokenType . toS)
