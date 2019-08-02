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

module EdgeNode.Model.User.Qualification (StateExamination) where

import EdgeNode.User.Qualification

import Database.Groundhog.TH.Extended
import Orphan ()
import TH.Generator
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Database.Groundhog.Instances ()
import Control.Lens.Iso.Extended

mkPersist_ [groundhog| 
 - entity: StateExamination
   schema: edgeNode
 |]

derivePrimitivePersistField ''Country [| jsonb |]
derivePrimitivePersistField ''Name [| jsonb |]
derivePrimitivePersistField ''Provider [| jsonb |]
deriveWrappedPrimitivePersistField ''StateExamination_ProviderWrapper