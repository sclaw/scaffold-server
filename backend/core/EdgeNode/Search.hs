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
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE InstanceSigs  #-}

module EdgeNode.Search (SearchPiece (..)) where

import TH.Generator
import Orm.PersistField ()
import Orphan ()
import GHC.Generics

data SearchPiece = All | Qualification 
  deriving stock Generic
  deriving stock Show
  deriving stock Read

mkParamSchemaEnum ''SearchPiece
mkFromHttpApiDataEnum ''SearchPiece