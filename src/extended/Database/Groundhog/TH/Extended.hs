{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Groundhog.TH.Extended
  ( module Database.Groundhog.TH
  , myCodegenConfig
  , mkPersist_
  , mkPersist__
  , short
  , long
) where

import           Control.Lens
import qualified Data.Text as T

import Data.Aeson.Lens (key, _Integral)
import Data.List (isPrefixOf)
import Data.List.Extra (splitOn)
import Data.Maybe (fromMaybe)
import Data.Yaml (Value, decodeFileEither)
-- To bring instances. Anyone using 'mkPersist_' want
-- generated code to compile, and it requires instances
-- from Database.Groundhog.
import Database.Groundhog ()

import Database.Groundhog.TH
import Database.Groundhog.TH.Settings
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

removeLeadingUnderscore :: String -> String
removeLeadingUnderscore ('_':s) = s
removeLeadingUnderscore s       = s

stripLast :: Int -> String -> String
stripLast n s = take (length s - n) s

removePrefix :: String -> String -> String
removePrefix p s = if p `isPrefixOf` s then drop (length p) s else s

myCodegenConfig :: CodegenConfig
myCodegenConfig =
  let NamingStyle {..} = suffixNamingStyle
      myMkExprFieldName d c cix f
        = stripLast 4 . mkExprFieldName d c cix (removeLeadingUnderscore f)
      myMkDbFieldName d c cix f
        = mkDbFieldName d c cix (removeLeadingUnderscore f)
      myMkExprSelectorName d c f
        = stripLast 7 . mkExprSelectorName d c (removeLeadingUnderscore f)
      myNamingStyle = suffixNamingStyle {
        mkExprFieldName    = myMkExprFieldName,
        mkDbFieldName      = myMkDbFieldName,
        mkExprSelectorName = myMkExprSelectorName
      }
    in defaultCodegenConfig { namingStyle = myNamingStyle }

myCodegenConfig2 :: CodegenConfig
myCodegenConfig2 =
  let ns@NamingStyle {..} = namingStyle myCodegenConfig
      newMkExprFieldName d c cix f
        = removePrefix d . mkExprFieldName d c cix f
      newMkExprSelectorName d c f
        = removePrefix d . mkExprSelectorName d c f
      new = ns { mkExprFieldName = newMkExprFieldName
               , mkExprSelectorName = newMkExprSelectorName
               }
  in  myCodegenConfig { namingStyle = new }

short, long :: CodegenConfig
short = myCodegenConfig2
long  = myCodegenConfig

mkPersist_ :: PersistDefinitions -> Q [Dec]
mkPersist_ = mkPersist myCodegenConfig

mkPersist__ :: CodegenConfig -> PersistDefinitions -> Q [Dec]
mkPersist__ config defs@PersistDefinitions {psEntities} = do
  cmodule  <- loc_module <$> location
  let parts = splitOn "." cmodule
      prefix = concat . drop 2 $ splitOn "." cmodule
  addDependentFile "migration.yaml"
  Right (versions :: Value) <- runIO $ decodeFileEither "migration.yaml"
  let kpath   = foldr1 (.) $ map (key . T.pack) parts
      version = fromMaybe 0 $ versions ^? kpath._Integral
      entries = map fn psEntities
      fn e = e { psDbEntityName = Just dbname }
        where
          dataname = psDataName e
          dbname  = fromMaybe (concat [prefix, dataname, show version])
                              (psDbEntityName e)

  mkPersist config (defs{psEntities = entries})
