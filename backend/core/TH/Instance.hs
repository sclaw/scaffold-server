{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TH.Instance
       ( derivePrimitivePersistField
       , deriveWrappedPrimitivePersistField
       , deriveNumInstanceFromWrappedNum
       , deriveWrappedForDataWithSingleField
       , deriveToSchemaAndJSON
       , deriveToSchemaAndJSONProtoIdent
       , deriveToSchemaAndJSONProtoEnum
       )
       where

import Control.Lens
import Database.Groundhog.Core
import Language.Haskell.TH
import Data.Aeson.Extended          (deriveJSON')
import Data.Swagger.Schema.Extended (deriveToSchema)
import Data.Aeson
import Data.Swagger
import Data.Scientific as Scientific
import Data.Maybe (fromMaybe)
import Data.Proxy
import Control.Lens.Iso.Extended
import Data.Text (stripPrefix)
import Data.Char (toUpper)

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

{- 
   insofar as proto3-suite generates instances for both Aeson and Swagger
   and its appearences are bizarre.
   for instance let's take a look at this association: UserId { userIdIdent :: Int } -> { userIdIdent: 0} 
   we no need to hold unnecessary field used as accessor within type. 
   this deriviation solves this issue, at one this alleviates its using on client side
   drawback: original type is slighty changed, we should add postfix Wrapper for 
   2 tantamount instances of same type: from proto3-suite and our
 -}  
deriveToSchemaAndJSONProtoIdent :: Name -> Q [Dec]
deriveToSchemaAndJSONProtoIdent name =
  do let nameT = conT name
     let i = mkName "i"
     TyConI (DataD _ _ _ _ [c] _) <- reify name
     let contructor = getConstructorPat c
     let s = nameBase name
     let wrapper = mkName $ s <> "Wrapper"
     let unwrap = mkName "unwrap"
     let show = mkName "Show"
     let entiityWrapper = 
          NewtypeD [] wrapper [] Nothing 
          (RecC wrapper 
           [(unwrap
           , Bang NoSourceUnpackedness 
                  NoSourceStrictness
           , ConT name)]) 
          [DerivClause (Just StockStrategy) [ConT show]]
     xs <- [d| 
        instance ToJSON $(conT wrapper) where
          -- example: Number (Scientific.sc ientific (fromIntegral i) 0)
          toJSON $(conP wrapper [conP contructor [varP i]]) =  
            Number (Scientific.scientific (fromIntegral $(varE i)) 0)    
                            
        instance FromJSON $(conT wrapper) where
          -- withScientific "UserId" $ 
          -- fmap (fromMaybe err) 
          -- . traverse (return . UserId) 
          -- . Scientific.toBoundedInteger
          -- where err = error "json parser: userId"
          parseJSON = 
            withScientific s $
            fmap (fromMaybe err) 
            . traverse 
              ( return 
              . $(conE wrapper) 
              . $(conE contructor)) 
            . Scientific.toBoundedInteger
            where err = error $ "json parser: " <> s
          
        instance ToSchema $(conT wrapper) where
          -- schema <- declareSchema (Proxy :: Proxy Int)
          -- return $ NamedSchema (Just "UserId") schema
          declareNamedSchema _ = do
            schema <- declareSchema (Proxy :: Proxy Int)
            return $ NamedSchema (Just s) schema
      |]
     return $ entiityWrapper : xs 

deriveToSchemaAndJSONProtoEnum :: Name -> String -> Q [Dec]
deriveToSchemaAndJSONProtoEnum name postfix = 
  do TyConI (DataD ctx n xs kind ys cl) <- reify name
     let new = mkName $ nameBase name <> postfix
     let geni = DerivClause Nothing [ConT (mkName "Generic")]
     let showi = DerivClause (Just StockStrategy) [ConT (mkName "Show")]
     let capitalizeHead x = x & _head %~ toUpper 
     let purgeNamePrefix (NormalC n xs) = 
          ((`NormalC` xs) . mkName . capitalizeHead . (^.from stext)) `fmap`
          stripPrefix 
          (nameBase name^.stext) 
          (nameBase n^.stext)
     let err = error $ "error: " <> show name     
     let ys' = map (fromMaybe err . purgeNamePrefix) ys
     return [DataD ctx new xs kind ys' ([geni, showi] ++ cl)]