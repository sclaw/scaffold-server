{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module TH.Instance
       ( derivePrimitivePersistField
       , deriveWrappedPrimitivePersistField
       , deriveNumInstanceFromWrappedNum
       , deriveWrappedForDataWithSingleField
       , deriveToSchemaAndJSON
       , deriveToSchemaAndJSONProtoIdent
       )
       where

import Control.Lens
import Database.Groundhog.Core
import Language.Haskell.TH
import Data.Aeson.Extended          (deriveJSON')
import Data.Swagger.Schema.Extended (deriveToSchema)
import Data.Aeson
import Data.Swagger

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
  where nameT = conT name

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

getConstructorType (RecC _ [(_, _, ConT c)]) = c
getConstructorType (NormalC _ [(_, ConT c)]) = c
getConstructorType _ = error "data not supported"
  
getConstructorPat (RecC c _) = c
getConstructorPat (NormalC c _) = c
getConstructorPat _ = error "data not supported"

deriveWrappedForDataWithSingleField :: Name -> Q [Dec]
deriveWrappedForDataWithSingleField name =
  do
    TyConI (DataD _ _ _ _ [c] _) <- reify name
    let constructor = getConstructorType c
    let x = mkName "x"
    let t = mkName $ nameBase name
    let from = lamE [conP t [varP x]] (varE x)
    let to = lamE [varP x] (appE (conE t) (varE x))
    [d|
      instance Wrapped $(conT t) where
        type Unwrapped $(conT t) = $(conT constructor)
        _Wrapped' = iso $from $to
     |]        

deriveToSchemaAndJSON :: Name -> Q [Dec]
deriveToSchemaAndJSON name = do
  xs <- deriveJSON' name
  ys <- deriveToSchema name
  return (xs ++ ys)

deriveToSchemaAndJSONProtoIdent :: Name -> Q [Dec]
deriveToSchemaAndJSONProtoIdent name =
  do let nameT = conT name
     let i = mkName "i"
     TyConI (DataD _ _ _ _ [c] _) <- reify name
     let contructor = getConstructorPat c
     [d| 
        instance ToJSON $nameT where
          -- example: Number (Scientific.scientific (fromIntegral i) 0)
          toJSON $(conP contructor [varP i]) = undefined   
        
        instance FromJSON $nameT where
          -- withScientific "UserId" $ 
          -- fmap (fromMaybe err) 
          -- . traverse (return . UserId) 
          -- . Scientific.toBoundedInteger
          -- where err = error "json parser: userId"
          parseJSON = undefined
          
          
        instance ToSchema $nameT where
          -- schema <- declareSchema (Proxy :: Proxy Int)
          -- return $ NamedSchema (Just "UserId") schema
          declareNamedSchema _ = undefined
      |]
