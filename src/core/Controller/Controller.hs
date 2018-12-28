{-# LANGUAGE TemplateHaskell #-}

module Controller.Controller (execQuery) where

import           Api
import           Katip
import           KatipHandler
import           Servant


execQuery :: ServerT AppApi KatipHandler
execQuery = const [1, 2, 4] `fmap` $(logTM) DebugS "getAllIntegers"