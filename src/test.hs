{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import TH.Mk
import Control.Lens
import Hasql.Statement
import Hasql.Encoders 
import Data.HList.HList
-- import Data.Profunctor.Unsafe
-- import Data.Generics.Product.Positions

newtype I = I Int deriving Show

data Inner = Inner { innerField1 :: String, innerField2 :: Int } deriving Show

data Test = Test { testField1 :: Int, testField2 :: I, testField3 :: Inner }

data Test1 = Test1 { testField11 :: Int, testField21 :: String }

mkEncoder ''Test
mkEncoder ''Test1
mkEncoder ''Inner


r = mkEncoderTest $ Test 1 (I 45) $ Inner "sdvdf" 5 

i = r^._3.to mkEncoderInner._1

s :: Int
s = r^._2.coerced

-- st :: Statement Test ()
-- st = lmap mkTpl $ Statement undefined undefined undefined False

mkTpl x = 
  mkEncoderTest x
  & _1 %~ (hEnd . hBuild)
  & _2 %~ (hEnd . hBuild . (^.coerced @_ @_ @Int @_)) 
  & _3 %~ (hFromTuple . mkEncoderInner)


ty = (mkTpl $ Test 1 (I 45) $ Inner "sdvdf" 5)^.from hTuple'