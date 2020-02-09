{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}



import Data.Aeson.WithField.Extended
import Data.Generics.Product.Positions
import Control.Lens


t = WithField (Just (1 :: Int)) (OptField (WithField (Nothing :: Maybe String) (1 :: Int)))

r = t^.position @2.position @1.position @2
