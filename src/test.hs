{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


import Data.Yaml
import Data.Aeson

import EdgeNode.Country
import Data.Aeson.WithField
import qualified Data.Text as T
import TH.Proto

test = fmap () decodeFileEither @[(WithField "enum" Country (Maybe T.Text))] "../enum/country/english.yaml"
