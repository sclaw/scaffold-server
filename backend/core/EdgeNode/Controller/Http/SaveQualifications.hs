{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.SaveQualifications (controller) where

import EdgeNode.Error
import EdgeNode.Api.Http.User.SaveQualifications
import EdgeNode.User.Qualification
import EdgeNode.Category

import RetrofitProto
import Katip
import KatipController
import ReliefJsonData
import Data.Generics.Product
import Control.Lens
import qualified Data.Text as T
import Database.Action
import Data.Either.Unwrap
import Control.Lens.Iso.Extended
import Data.Vector.Lens
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import Data.String.Interpolate
import Data.List
import Contravariant.Extras.Contrazip
import Data.Int
import Data.Typeable
import Data.Maybe

controller :: SaveQualificationsRequest -> KatipController (Alternative (Error T.Text) SaveQualificationsResponse)
controller req =
  do 
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- flip runTryDbConnHasql raw $ \_ -> do 
      let xs = req^._Wrapped'.field @"requestValues".from vector
      let xs' = mapMaybe mkTpl xs
      let sql = 
            [i|insert into "edgeNode"."#{show (typeOf (undefined :: UserQualification))}"
               ("categoryType", "categoryKey", "providerKey", "qualificationKey") 
               (select * from unnest($1, $2, $3, $4)) returning id
            |]
      let vector v = HE.param (HE.array (HE.dimension foldl' (HE.element v)))
      let encoder = 
            unzip4 xs' >$
            contrazip4 
            (vector HE.text) 
            (vector HE.int8) 
            (vector HE.int8) 
            (vector HE.int8)
      let decoder = HD.rowList $ HD.column HD.int8 <&> UserQualificationId
      Hasql.Session.statement () (HS.Statement sql encoder decoder False) 
    whenLeft x ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)
    let mkResp = SaveQualificationsResponse . Response . (^.vector)     
    return $ bimap mkErr mkResp x^.eitherToAlt

mkTpl :: UserQualification -> Maybe (T.Text, Int64, Int64, Int64)
mkTpl x = 
  do  
    cat <- x^?field @"userQualificationCategory"._Just
    let (catType, catId) = 
          case cat of
            UserQualificationCategoryStateExamId (StateExamId i) -> 
             (fromType TypeStateExam^.stext, i)
            UserQualificationCategoryHigherDegreeId (HigherDegreeId i) -> 
             (fromType TypeHigherDegree^.stext, i)
            UserQualificationCategoryInternationalDiplomaId (InternationalDiplomaId i) -> 
             (fromType TypeInternationalDiploma^.stext, i)
            UserQualificationCategoryLanguageStandardId (LanguageStandardId i) -> 
             (fromType TypeLanguageStandard^.stext, i)
    provider <- x^?field @"userQualificationProviderIdent"._Just.field @"providerIdValue"
    qual <- x^?field @"userQualificationQualificationIdent"._Just.field @"qualificationIdValue"
    return (catType, catId, provider, qual)