{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Transport.Validator
       ( qualificationBuilder
       , degreeToValues
       , qualififcationPatch
       , registration
       , feedback
       , regeneratePassword
       , MandatoryField (..)
       , NewAccountError (..)
       , newAccount
       , RegeneratePasswordError (..)
       ) where

import EdgeNode.Transport.Provider.Qualification
import EdgeNode.Transport.Error
import EdgeNode.Transport.Auth
import EdgeNode.Transport.Feedback
import EdgeNode.Transport.Provider.Settings
import EdgeNode.Transport.Rbac

import TH.Mk
import Validation
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Maybe
import Control.Lens
import Data.Generics.Product.Fields
import qualified Data.Vector as V
import TextShow
import qualified Text.RE.PCRE.Text as RegExp
import Control.Lens.Iso.Extended
import Data.Foldable
import Data.Coerce
import Proto3.Suite.Types
import Data.Int
import Data.String.Conv

mkEncoder ''TuitionFees
mkEncoder ''FeesInfo

data QualificationBuilderError =
       TitleEmpty
     | AcademicAreaEmpty
     | DegreeValueNotFoundAtQualificationDegree
     | QualificationNotFound
     | TuitionFeesDuplicated
     deriving stock Show

instance AsError QualificationBuilderError where
  asError TitleEmpty = asError @T.Text "qualification title shouldn't be empty"
  asError AcademicAreaEmpty = asError @T.Text "at least one academic area should be set"
  asError DegreeValueNotFoundAtQualificationDegree = asError @T.Text "degree value mismatches given qualification degree"
  asError QualificationNotFound = asError @T.Text "qualification not found"
  asError TuitionFeesDuplicated = asError @T.Text "some of tuition fees is duplicated, fields currency, period, countries should be uniqie within list"

qualificationBuilder :: QualificationBuilder -> Validation [QualificationBuilderError] ()
qualificationBuilder builder
  | isNothing (
      builder^.
      field @"qualificationBuilderQualification")
    = Failure [QualificationNotFound]
qualificationBuilder builder =
  checkTitle *>
  checkAcademicArea *>
  checkDegreeValue *>
  checkTuitionFees
  where
    checkTitle
      | LT.null (
        builder^?!
        field @"qualificationBuilderQualification".
        _Just.
        field @"qualificationTitle")
        = Failure [TitleEmpty]
      | otherwise = pure ()
    checkAcademicArea
      | V.null (
        builder^?!
        field @"qualificationBuilderQualification".
        _Just.
        field @"qualificationAreas")
        = Failure [AcademicAreaEmpty]
      | otherwise = pure ()
    checkDegreeValue
      | not $ checkDegreeValue' (builder^?!field @"qualificationBuilderQualification"._Just)
        = Failure [DegreeValueNotFoundAtQualificationDegree]
      | otherwise = pure ()
    checkTuitionFees
      | not (checkDuplicatedFees  mkEncoderTuitionFees (builder^?!field @"qualificationBuilderFees")) = Success ()
      | otherwise = Failure [TuitionFeesDuplicated]

checkDegreeValue' :: Qualification -> Bool
checkDegreeValue' Qualification  {..} = check qualificationDegreeValue
  where
    degree = qualificationDegreeType^?!field @"enumerated"._Right
    check Nothing = True
    check (Just x) = elem (x^.field @"stringValue") (fromJust (lookup degree degreeToValues))

degreeToValues :: [(QualificationDegree, [LT.Text])]
degreeToValues =
  [ (QualificationDegreeUnifiedStateExam, map (LT.fromStrict . showt) [1 .. 100 :: Int])
  , (QualificationDegreeAdvancedLevelGCE, ["A*", "A", "B", "C", "D", "E"])
  , (QualificationDegreeMagistr, ["Maj", "Std"])
  , (QualificationDegreeBakalavr, ["Maj", "Std"])
  , (QualificationDegreeBSc, ["1", "2:1", "2:2", "3"])
  , (QualificationDegreeBA, ["1", "2:1", "2:2", "3"])
  , (QualificationDegreeMSc, ["1", "2:1", "2:2", "3"])
  , (QualificationDegreeMA, ["1", "2:1", "2:2", "3"])
  , (QualificationDegreeMRes, ["1", "2:1", "2:2", "3"])
  , (QualificationDegreeToeflIBT, map (LT.fromStrict . showt) [0 .. 120 :: Int])
  , (QualificationDegreeToeflPBT, map (LT.fromStrict . showt) [310 .. 677 :: Int])
  , (QualificationDegreeTestDeConnaissanceDuFrançaisTCF, ["A1", "A2", "B1", "B2", "C1", "C2"])
  , (QualificationDegreeIELTS, map (LT.fromStrict . showt) [0, 0.5 .. 9 :: Double])]


data PatchQualificationError = PatchQualificationErrorTuitionFeesDuplicated
  deriving stock Show

instance AsError PatchQualificationError where
  asError PatchQualificationErrorTuitionFeesDuplicated =
    asError @T.Text "some of tuition fees is duplicated, fields currency, period, countries should be uniqie within list"

qualififcationPatch :: PatchQualification -> Validation [PatchQualificationError] ()
qualififcationPatch patch = checkFees $ patchQualificationTuitionFees patch
  where
    checkFees (Just (PatchTuitionFees xs)) = do
      let xs' = V.mapMaybe patchTuitionFees_FeesValue xs
      if not (checkDuplicatedFees mkEncoderFeesInfo xs')
      then Success ()
      else Failure [PatchQualificationErrorTuitionFeesDuplicated]
    checkFees Nothing = Success ()

{-
password validation:
  Minimum eight characters, at least one letter and one number: ^(?=.*[A-Za-z])(?=.*\d)[A-Za-z\d]{8,}$
  Minimum eight characters, at least one letter, one number and one special character: ^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{8,}$
  Minimum eight characters, at least one uppercase letter, one lowercase letter and one number: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d]{8,}$
  Minimum eight characters, at least one uppercase letter, one lowercase letter, one number and one special character: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$
  Minimum eight and maximum 10 characters, at least one uppercase letter, one lowercase letter, one number and one special character: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,10}$
email validation - ^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+[.]{1}[a-zA-Z0-9-.]{2,}$
-}

data RegistrationError =
       RegistrationErrorEmailNotValid
     | RegistrationErrorPasswordWeak
     | RegistrationErrorPasswordsMismatch
      deriving stock Show

instance AsError RegistrationError where
  asError RegistrationErrorEmailNotValid = asError @T.Text "email not valid"
  asError RegistrationErrorPasswordWeak = asError @T.Text "password error. Minimum eight characters, at least one uppercase letter, one lowercase letter and one number"
  asError RegistrationErrorPasswordsMismatch = asError @T.Text "passwords mismatch"

passwordValidation password = RegExp.matched (password RegExp.?=~ [RegExp.re|^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d]{8,}$|])
emailValidation email = RegExp.matched (email RegExp.?=~ [RegExp.re|^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+[.]{1}[a-zA-Z0-9-.]{2,}$|])

registration :: Registration -> Validation [RegistrationError] ()
registration registration = checkEmail *> checkPaswwordStrength *> checkPassswordsMatch
  where
    checkEmail
      | emailValidation
        (registrationEmail registration^.lazytext)
        = Success ()
      | otherwise = Failure [RegistrationErrorEmailNotValid]
    checkPaswwordStrength
      | passwordValidation
        (registrationPassword registration^.lazytext)
        = Success ()
      | otherwise = Failure [RegistrationErrorPasswordWeak]
    checkPassswordsMatch
      | registrationPassword registration ==
        registrationPasswordOnceAgain registration
        = Success ()
      | otherwise = Failure [RegistrationErrorPasswordsMismatch]

checkDuplicatedFees :: (Eq f, Eq e, Eq d) => (a -> ((c, d, e, f))) -> V.Vector a -> Bool
checkDuplicatedFees mkTpl = fst . V.foldr check (False, []) . uncurry4 (V.zipWith4 (\_ c p cs -> (c, p, cs))) . V.unzip4 . V.map mkTpl
  where
    check x y | x `elem` (y^._2) = y & _1 .~ True & _2 %~ (x:)
              | otherwise = y & _2 %~ (x:)
    uncurry4 f (x, y, z, w) = f x y z w

newtype MandatoryField = MandatoryField T.Text deriving stock Show

instance AsError MandatoryField where
  asError (MandatoryField field) = asError @T.Text $ field <> " field is mandatory"

-- | Feedback
--
-- >>> import Proto3.Suite.Types
-- >>> feedback $ Feedback Nothing mempty (Enumerated (Right ReasonTroubleWithAccount))  mempty  mempty
-- Failure [MandatoryField "who",MandatoryField "message",MandatoryField "email"]
feedback :: Feedback -> Validation [MandatoryField] ()
feedback feedback = sequenceA_
  [ if LT.null (feedbackWho feedback) then Failure [MandatoryField "who"] else Success ()
  , if LT.null (feedbackMessage feedback) then Failure [MandatoryField "message"] else Success ()
  , if LT.null (feedbackEmail feedback) then Failure [MandatoryField "email"] else Success ()
  ]

data NewAccountError =
       NewAccountErrorEmail
     | NewAccountErrorRole
     | NewAccountErrorTaken
     deriving stock Show

instance AsError NewAccountError where
  asError NewAccountErrorEmail = asError @T.Text "email wrong"
  asError NewAccountErrorRole = asError @T.Text "available role: editor, guest"
  asError NewAccountErrorTaken = asError @T.Text "email taken"

newAccount :: NewAccount -> Validation [NewAccountError] (T.Text, Role)
newAccount account =
  (,) <$>
  checkEmail (newAccountEmil account^.lazytext) <*>
  checkRole (coerce @_ @(Either Int32 Role) (newAccountRole account))
  where
    checkEmail email
      | emailValidation email = Success email
      | otherwise = Failure [NewAccountErrorEmail]
    checkRole (Right r)
      | r == RoleEditor || r == RoleGuest = Success r
      | otherwise = Failure [NewAccountErrorRole]
    checkRole (Left _) = Failure [NewAccountErrorRole]

data RegeneratePasswordError =
       RegeneratePasswordWeak
     | RegeneratePasswordMismatched
     | RegeneratePasswordTokenNF
     | RegeneratePasswordTokenUsed
     | RegeneratePasswordSamePass
     deriving stock Show

instance AsError RegeneratePasswordError where
  asError RegeneratePasswordWeak = asError @T.Text "password weak"
  asError RegeneratePasswordMismatched = asError @T.Text "passwords mismatched"
  asError RegeneratePasswordTokenNF = asError @T.Text "token not found"
  asError RegeneratePasswordTokenUsed = asError @T.Text "token already used"
  asError RegeneratePasswordSamePass = asError @T.Text "new password is matched with old one"

regeneratePassword :: RegeneratePassword -> Validation [RegeneratePasswordError] ()
regeneratePassword RegeneratePassword {..} = sequenceA_
  [ if regeneratePasswordPassword /=
       regeneratePasswordOnceAgainPassword
    then Failure [RegeneratePasswordMismatched]
    else Success ()
  , if passwordValidation
       (toS regeneratePasswordPassword)
     then Success ()
    else Failure [RegeneratePasswordWeak]
  ]