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

module EdgeNode.Model.User.Qualification 
       ( StateExamination
       , coercedCountry
       , coercedName
       , coercedProvider
       ) where

import EdgeNode.User.Qualification

import Database.Groundhog.TH.Extended
import Orphan ()
import TH.Generator
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Database.Groundhog.Instances ()
import Control.Lens
import Proto3.Suite.Types
import Data.Either

mkPersist_ [groundhog| 
 - entity: StateExamination
   schema: edgeNode
 |]

enumConvertor ''Country
enumConvertor ''Name
enumConvertor ''Provider
deriveWrappedPrimitivePersistField ''StateExamination_ProviderWrapper
derivePrimitivePersistField ''Country [| iso fromCountry toCountry |]
derivePrimitivePersistField ''Name [| iso fromName toName |]
derivePrimitivePersistField ''Provider [| iso fromProvider toProvider |]

coercedCountry :: Enumerated Country -> Country
coercedCountry x = x^.(coerced :: Iso' (Enumerated Country) (Either Int Country)).to (fromRight undefined)

coercedName :: Enumerated Name -> Name
coercedName x = x^.(coerced :: Iso' (Enumerated Name) (Either Int Name)).to (fromRight undefined)

coercedProvider :: Enumerated Provider -> Provider
coercedProvider x = x^.(coerced :: Iso' (Enumerated Provider) (Either Int Provider)).to (fromRight undefined)