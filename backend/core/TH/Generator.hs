{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module TH.Generator
       ( derivePrimitivePersistField
       , deriveWrappedPrimitivePersistField
       , deriveNumInstanceFromWrappedNum
       , deriveWrappedForDataWithSingleField
       , deriveToSchemaAndJSON
       , deriveToSchemaAndJSONProtoIdent
       , deriveToSchemaAndJSONProtoEnum
       , deriveToSchemaAndDefJSON
       , enumConvertor
       , derivePrimitivePersistFieldParam
       , requestWrapper
       , responseWrapper
       , mkFromHttpApiDataIdent
       , mkFromHttpApiDataEnum
       , mkParamSchemaEnum
       )
       where

import Control.Lens
import Database.Groundhog.Core
import Language.Haskell.TH
import Data.Aeson.Extended          (deriveJSON', deriveJSON)
import Data.Swagger.Schema.Extended
import Data.Aeson
import Data.Swagger
import Data.Scientific as Scientific
import Data.Maybe (fromMaybe)
import Data.Proxy
import Control.Lens.Iso.Extended
import Data.Text (stripPrefix, split)
import Text.Casing (quietSnake)
import Servant.API
import Data.Char
import qualified Data.Text as T

derivePrimitivePersistField :: Name -> ExpQ -> Q [Dec]
derivePrimitivePersistField name iso = [d|
  instance PersistField $nameT where
    persistName _ = $(litE.stringL.show $ name)
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType proxy value = dbType proxy (view $iso value)
  instance NeverNull $nameT
  instance PrimitivePersistField $nameT where
    toPrimitivePersistValue value = 
      toPrimitivePersistValue (view $iso value)
    fromPrimitivePersistValue prim = 
      view (from $iso) (fromPrimitivePersistValue prim)
  |]
  where nameT = conT name

derivePrimitivePersistFieldParam :: Name -> ExpQ -> Q [Dec]
derivePrimitivePersistFieldParam name iso = 
  [d|
     instance (PrimitivePersistField $(varT a)
             , ToJSON $(varT a)
             , FromJSON $(varT a)) 
             => PersistField $(appT nameT (varT a)) where
       persistName _ = $(litE.stringL $ nameBase name)
       toPersistValues = primToPersistValue
       fromPersistValues = primFromPersistValue
       dbType proxy value = dbType proxy (view $iso value)
     instance NeverNull $(appT nameT (varT a))
     instance (PrimitivePersistField $(varT a)
             , ToJSON $(varT a)
             , FromJSON $(varT a)) 
             => PrimitivePersistField $(appT nameT (varT a)) where
       toPrimitivePersistValue value = 
         toPrimitivePersistValue (view $iso value)
       fromPrimitivePersistValue prim = 
         view (from $iso) (fromPrimitivePersistValue prim)
  |]
  where nameT = conT name
        a = mkName "a"  

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
  x <- deriveJSON' name
  y <- deriveToSchema name
  return $ x ++ y

deriveToSchemaAndDefJSON :: Name -> Q [Dec]
deriveToSchemaAndDefJSON name = do
  x <- deriveJSON defaultOptions name
  y <- deriveToSchemaDef name
  return $ x ++ y

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
deriveToSchemaAndJSONProtoEnum name prefix = 
  do TyConI (DataD ctx n xs kind ys cl) <- reify name
     let new = mkName $ prefix <> nameBase name
     let geni = DerivClause Nothing [ConT (mkName "Generic")]
     let showi = DerivClause (Just StockStrategy) [ConT (mkName "Show")]
     let read = DerivClause (Just StockStrategy) [ConT (mkName "Read")]
     let capitalizeHead x = x & _head %~ toUpper 
     let purgeNamePrefix (NormalC n xs) = 
          ((`NormalC` xs) . mkName . capitalizeHead . (^.from stext)) `fmap`
          stripPrefix 
          (nameBase name^.stext) 
          (nameBase n^.stext)
     let err = error $ "error: " <> show name     
     let ys' = map (fromMaybe err . purgeNamePrefix) ys
     return [DataD ctx new xs kind ys' ([geni, showi, read] ++ cl)]

enumConvertor :: Name -> Q [Dec]
enumConvertor name = 
  do TyConI (DataD _ _ _ _ xs _) <- reify name
     let str = mkName "String"
     let isoNFrom = mkName ("from" <> nameBase name)
     let mkClauseFrom (NormalC n _) = 
          let n' = (quietSnake . nameBase) n  
          in Clause 
              [ConP n []] 
              (NormalB (LitE (StringL n'))) []
     let fromSig = 
          SigD 
           isoNFrom 
           (AppT (AppT ArrowT (ConT name)) (ConT str))                
     let from = FunD isoNFrom (map mkClauseFrom xs)
     let isoNTo = mkName ("to" <> nameBase name)
     let mkClauseTo (NormalC n _) =
          let n' = (quietSnake . nameBase) n
          in Clause 
              [LitP (StringL n')]
              (NormalB (ConE n)) []
     let toSig = 
          SigD 
           isoNTo 
           (AppT (AppT ArrowT (ConT str)) (ConT name))         
     let to = FunD isoNTo (map mkClauseTo xs)
     let isoN = mkName ("iso" <> nameBase name)
     let iso = mkName "Iso'"
     let isoSig = SigD isoN (AppT (AppT (ConT iso) (ConT name)) (ConT str))  
     iosDec <- [d| $(varP isoN) = $(appE (appE (varE (mkName "iso")) (varE isoNFrom)) (varE isoNTo)) |]
     return $ [fromSig, from, toSig, to, isoSig] ++ iosDec

requestWrapper :: Name -> Q [Dec]
requestWrapper name = do
  TyConI (DataD _ n _ _ _ _) <- reify name
  let Just prefix = fmap (last . (^.stext.to (split (== '.')))) $ nameModule n
  let wrapper = mkName $ (prefix^.from stext) <> nameBase name
  let fromJson = mkName "FromJSON"
  let toSchema = mkName "ToSchema"
  let gen = mkName "Generic"
  let show = mkName "Show"
  let entiityWrapper = 
        NewtypeD [] wrapper [] Nothing 
        (NormalC wrapper 
        [(Bang NoSourceUnpackedness 
                NoSourceStrictness
        , ConT name)]) 
        [ DerivClause (Just NewtypeStrategy) [ConT fromJson]
        , DerivClause (Just AnyclassStrategy) [ConT toSchema]
        , DerivClause (Just StockStrategy) [ConT gen]
        , DerivClause (Just NewtypeStrategy) [ConT show]
        ]
  let x = mkName "x"      
  inst <- 
   [d| instance Wrapped $(conT wrapper) where
         type Unwrapped $(conT wrapper) = $(conT name) 
         _Wrapped' = iso fromWrapper toWrapper
          where fromWrapper $(conP wrapper [varP x]) = $(varE x)
                toWrapper $(varP x) = $(appE (conE wrapper) (varE x)) 
    |]                  
  return $ entiityWrapper : inst 

responseWrapper :: Name -> Q [Dec]
responseWrapper name =  do
  TyConI (DataD _ n _ _ _ _) <- reify name
  let Just prefix = fmap (last . (^.stext.to (split (== '.')))) $ nameModule n
  let wrapper = mkName $ (prefix^.from stext) <> nameBase name
  let toJson = mkName "ToJSON"
  let toSchema = mkName "ToSchema"
  let gen = mkName "Generic"
  let show = mkName "Show"
  let entiityWrapper = 
        NewtypeD [] wrapper [] Nothing 
        (NormalC wrapper 
        [(Bang NoSourceUnpackedness 
                NoSourceStrictness
        , ConT name)]) 
        [ DerivClause (Just NewtypeStrategy) [ConT toJson]
        , DerivClause (Just AnyclassStrategy) [ConT toSchema]
        , DerivClause (Just StockStrategy) [ConT gen]
        , DerivClause (Just NewtypeStrategy) [ConT show]
        ]
  let x = mkName "x"      
  inst <- 
    [d| instance Wrapped $(conT wrapper) where
          type Unwrapped $(conT wrapper) = $(conT name) 
          _Wrapped' = iso fromWrapper toWrapper
           where fromWrapper $(conP wrapper [varP x]) = $(varE x)
                 toWrapper $(varP x) = $(appE (conE wrapper) (varE x)) 
    |]                          
  return $ entiityWrapper : inst

mkFromHttpApiDataIdent :: Name -> Q [Dec]
mkFromHttpApiDataIdent name = do
  let base = nameBase name
  let con = mkName base
  let read = mkName "read"
  [d| instance FromHttpApiData $(conT name) where
        parseUrlPiece x = 
          if all isNumber sx then
            Right $(appE (conE con) 
                    (appE (varE read) 
                     (varE (mkName "sx"))))  
          else Left $ "cannot convert " <> base 
          where sx = x^.from stext
   |]

mkFromHttpApiDataEnum :: Name -> Q [Dec]
mkFromHttpApiDataEnum name = do
  reified <- reify name
  let base = nameBase name
  [d| instance FromHttpApiData $(conT name) where
        parseUrlPiece :: T.Text -> Either T.Text $(conT name)
        parseUrlPiece x = Right $ x^.from stext.to (read . (& _head %~ toUpper))
   |]

mkParamSchemaEnum :: Name -> Q [Dec]
mkParamSchemaEnum name = do 
  TyConI (DataD _ _ _ _ xs _) <- reify name
  let mkTitle (NormalC n _) = nameBase n & _head %~ toLower
  let xs' = map mkTitle xs
  [d| instance ToParamSchema $(conT name) where
       toParamSchema _ = mempty & type_ .~ SwaggerString & enum_ ?~ xs'
   |]