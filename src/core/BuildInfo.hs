{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module BuildInfo (gitCommit, gitTag, location) where

import Data.String (fromString)
import Language.Haskell.TH (Exp(..), Lit(..))
import Language.Haskell.TH.Lib (ExpQ)
import Language.Haskell.TH.Syntax 
       (lift, loc_module, qLocation)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Data.Maybe

gitCommit :: ExpQ
gitCommit = lift $ unsafePerformIO $ (head.lines) `fmap` readProcess "git" ["log", "-1", "--format=%h"] mempty
 
gitTag :: ExpQ
gitTag = lift $ unsafePerformIO $ fromMaybe "-" . listToMaybe . lines <$> readProcess "git" ["tag", "--list", "--sort=-creatordate"] ""

location :: ExpQ
location = [| fromString $((LitE . StringL . loc_module) `fmap` qLocation) |]