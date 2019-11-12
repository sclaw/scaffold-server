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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Model.User.Qualification 
       (EdgeNode.Model.User.Qualification.UserQualification
       , UserQualificationSkill (..)
       , UserQualificationId (..)
       ) where

import EdgeNode.User.Qualification

import TH.Mk
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Database.Groundhog.TH.Extended ()
import Data.Swagger.ParamSchema
import Servant.API
import Control.Lens.Iso.Extended
import Control.Lens
import Data.Text.Read
import Data.Aeson.Extended
import Test.QuickCheck

data UserQualification

instance ToParamSchema UserQualificationId

instance Arbitrary UserQualificationId where
  arbitrary = UserQualificationId <$> arbitrary

instance FromHttpApiData UserQualificationId where
  parseUrlPiece x = bimap (^.stext) (UserQualificationId . fst) (decimal x)

mkWrappedPrimitivePersistField ''UserQualificationId
deriveJSON' ''UserQualificationSkill
deriveJSON' ''UserQualificationFullinfoSkill