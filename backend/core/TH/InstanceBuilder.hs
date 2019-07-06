{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TH.InstanceBuilder
       (
         derivePrimitivePersistField
       , deriveWrappedPrimitivePersistField
       , deriveNumInstanceFromWrappedNum
       , deriveWrappedForDataWithSingleField
       , deriveToSchemaAndJSON
       )
       where

import Control.Lens
import Database.Groundhog.Core
import Language.Haskell.TH
import Data.Aeson.Extended          (deriveJSON')
import Data.Swagger.Schema.Extended (deriveToSchema)


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

deriveNumInstanceFromWrappedNum :: Name -> ExpQ -> Q [Dec]
deriveNumInstanceFromWrappedNum name iso =
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

deriveWrappedForDataWithSingleField :: Name -> Q [Dec]
deriveWrappedForDataWithSingleField name =
  do
    TyConI (DataD _ _ _ _ [c] _) <- reify name
    let derive inner =
          do
            let x = mkName "x"
            let t = mkName $ nameBase name
            let from = lamE [conP t [varP x]] (varE x)
            let to = lamE [varP x] (appE (conE t) (varE x))
            [d|
               instance Wrapped $(conT t) where
                 type Unwrapped $(conT t) = $(conT inner)
                 _Wrapped' = iso $from $to
             |]
    case c of
      RecC _ [(_, _, ConT inner)] -> derive inner
      NormalC _ [(_, ConT inner)] -> derive inner
      _                           -> error "data not supported"

deriveToSchemaAndJSON :: Name -> Q [Dec]
deriveToSchemaAndJSON name = do
  xs <- deriveJSON' name
  ys <- deriveToSchema name
  return (xs ++ ys)
