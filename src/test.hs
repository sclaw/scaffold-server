{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


import Data.Yaml
import Data.Aeson

import EdgeNode.Country
import Data.Aeson.WithField
import qualified Data.Text as T

test = decodeFileEither @[(WithField "enum" CountryExt (Maybe T.Text))] "../enum/country/en.yaml"
