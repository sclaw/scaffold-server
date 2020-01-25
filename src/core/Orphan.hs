{-# LANGUAGE ScopedTypeVariables #-}

module Orphan () where
    
import Proto3.Suite.Types
import Data.Default.Class.Extended
import Data.Aeson hiding (json)

instance ToJSON a => ToJSON (Enumerated a) where
  toJSON (Enumerated (Right x)) = toJSON x
  toJSON (Enumerated _) = error "enumerated decode error" 
      
instance FromJSON a => FromJSON (Enumerated a) where 
  parseJSON x = (Enumerated . Right) `fmap` parseJSON x

instance Default a => Default (Enumerated a) where
  def = Enumerated $ Right def