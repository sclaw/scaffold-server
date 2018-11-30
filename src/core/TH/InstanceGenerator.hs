{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

----------------------------------------------------------------------------

-- Module      :  TH.InstanceGenerator
-- Maintainer  :  fclaw007@gmail.com
--
-- The "TH.InstanceGenerator" generate various type class instance

-----------------------------------------------------------------------------

module TH.InstanceGenerator
       ( mkEnumTypeRandomInstance
       , derivePrimitivePersistField
       , deriveWrappedPrimitivePersistField
       , deriveNumInstanceFromNumTypes
       )
       where

import Control.Lens
import Database.Groundhog.Core
import Language.Haskell.TH
import System.Random


mkEnumTypeRandomInstance :: Name -> Q [Dec]
mkEnumTypeRandomInstance name = [d|
  instance (Enum $a, Bounded $a) => Random $a where
      randomR (l :: $a, u) g = (toEnum i :: $a, g')
          where
              (i, g') = (fromEnum l, fromEnum u) `randomR` g
      random g = (minBound :: $a, maxBound :: $a) `randomR` g
  |]
  where
    a = conT name

derivePrimitivePersistField :: Name -> ExpQ -> Q [Dec]
derivePrimitivePersistField name iso = [d|
  instance PersistField $nameT where
    persistName _ = $(litE.stringL.show $ name)
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType proxy value = dbType proxy (view $iso value)
  instance NeverNull $nameT
  instance PrimitivePersistField $nameT where
    toPrimitivePersistValue value = toPrimitivePersistValue (view $iso value)
    fromPrimitivePersistValue prim = view (from $iso) (fromPrimitivePersistValue prim)
  |]
  where
    nameT = conT name

deriveWrappedPrimitivePersistField :: Name -> Q [Dec]
deriveWrappedPrimitivePersistField name = do
  decs1 <- makeWrapped name
  decs2 <- derivePrimitivePersistField name [| _Wrapped' |]
  pure $ decs1 ++ decs2

deriveNumInstanceFromNumTypes :: Name -> ExpQ -> Q [Dec]
deriveNumInstanceFromNumTypes name iso =
  do
    let x = mkName "x"
        y = mkName "y"
        plus = mkName "+"
        multiply = mkName "*"
        abs = mkName "abs"
        minus = mkName "-"
        sig = mkName "signum"
    TyConI (DataD _ _ _ _ [RecC cons _] _) <- reify name  
    [d|
       instance Num $(conT name) where
         (+) $(conP cons [varP x]) $(conP cons [varP y]) =
             $(conE cons) $(infixE (Just (varE x)) (varE plus) (Just (varE y)))
         (*) $(conP cons [varP x]) $(conP cons [varP y]) =
             $(conE cons) $(infixE (Just (varE x)) (varE multiply) (Just (varE y)))
         abs $(conP cons [varP x]) = $(conE cons) $(appE (varE abs) (varE x))
         signum $(conP cons [varP x]) = $(conE cons) $(appE (varE sig) (varE x))
         fromInteger x = $(conE cons) (view $iso x)
         (-) $(conP cons [varP x]) $(conP cons [varP y]) =
             $(conE cons) $(infixE (Just (varE x)) (varE minus) (Just (varE y)))
     |]