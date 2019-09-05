{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Http.GetQualififcations (controller) where

import EdgeNode.Error
import EdgeNode.Model.Provider
import EdgeNode.Model.Qualification
import EdgeNode.Api.Http.User.GetQualififcations

import Proto
import KatipController
import Json
import qualified Data.Text as T
import Data.Generics.Product
import Control.Lens
import Katip
import Katip.Monadic
import Data.Bifunctor
import Database.Action
import Control.Lens.Iso.Extended
import Data.Vector.Lens
import Data.Either.Unwrap
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.Int
import Data.String.Interpolate
import Control.Monad
import qualified Protobuf.Scalar as Proto
import GHC.Exts
import qualified Data.Vector as V
import qualified Data.Tree.Extended as Tree
import System.IO.Unsafe
import Data.Tree.Pretty
import Data.Maybe
import Data.Sort

controller :: ProviderId -> KatipController (Alternative (Error T.Text) GetQualififcationsResponse)
controller provider = maybe (return err) ok (provider^?field @"providerIdValue")
  where 
    err = Error $ ResponseError "provider empty"
    ok ident = 
      do
        $(logTM) DebugS (logStr ([i|provider id: #{show ident}|] :: String))
        raw <- fmap (^.katipEnv.rawDB) ask
        logtree <- katipAddNamespace (Namespace ["tree"]) askLoggerIO 
        x <- runTryDbConnHasql (action ident) raw
        whenLeft x ($(logTM) ErrorS . logStr . show) 
        let mkErr e = 
             ServerError $ 
             InternalServerError (show e^.stextl)
        let mkResp = 
               GetQualififcationsResponse 
             . Response 
             . (^.vector) 
             . map (uncurry Response_Value 
                  . second ((^.vector) . mkTrees logtree))     
        return $ bimap mkErr mkResp x^.eitherToAlt

type DegreeType = Proto.String
type Path = T.Text

action :: Int64 -> KatipLoggerIO -> Hasql.Session.Session [(Maybe DegreeType, [(Path, Node)])]
action ident logger =
  do 
    let sql = 
         [i|select 
             "qualificationProviderDegreeType",
             ltree2text(subpath(path, 0, 1)) as root, 
             array_agg (row(id, "qualificationProviderTitle",
             "qualificationProviderGradeRange", 
             ltree2text(path)) order by path)
            from "edgeNode"."QualificationProvider"
            where "qualificationProviderKey" = $1 
            group by root, "qualificationProviderDegreeType"           
         |]
    let encoder = ident >$ HE.param HE.int8
    let qualComposite = do 
          i <- HD.field HD.int8
          title <- HD.field HD.text
          grade <- fmap (^.from jsonb) <$> HD.nullableField HD.jsonb 
          path <- HD.field HD.text
          let mkRange xs = V.fromList 
               [EdgeNode.Model.Qualification.mkRange 
                exGradeRangeGrade 
                | ExGradeRange {..} <- 
                  sortOn exGradeRangeRank xs] 
          let qual = EdgeNode.Model.Qualification.Qualification 
                     (title^.from lazytext)
                     (maybe V.empty mkRange grade)
          return (path, Node (Just (QualificationId i)) (Just qual)) 
    let decoder = HD.rowList $ do
          degree <- (fmap (^.from lazytext.to Proto.String)) <$> 
                    HD.nullableColumn HD.text
          _ <- HD.column HD.text 
          xs <- HD.column 
           (HD.array 
            (HD.dimension replicateM 
             (HD.element 
              (HD.composite 
               qualComposite))))
          return (degree, xs)                 
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)
    
mkTrees :: KatipLoggerIO -> [(Path, Node)] -> [QualificationTree]
mkTrees logger xs = map (mkTree logger . snd) xs' 
  where 
    xs' = 
      [(GHC.Exts.the r, x) | x@(p, _) <- xs, 
       let r = T.takeWhile (/= '.') p, 
       then group by r using groupWith,
       then sortWith by p]

convert (Tree.Node n t) = QualificationTree (Just (snd n)) (map convert t^.vector) 

mkTree :: KatipLoggerIO -> [(Path, Node)] -> QualificationTree
mkTree logger (x:xs) = 
  unsafePerformIO $ do 
    logger DebugS (logStr (drawVerticalTreeWith 5 (fmap (^._1.from stext) qualtree)))
    return (convert qualtree)
  where put path (path', _) = T.isPrefixOf path' path
        qualtree = foldr (\x tree -> Tree.insert x (put (fst x)) tree) (Tree.Node x []) xs