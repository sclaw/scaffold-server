{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.User.Trajectory.Add (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.Qualification
import EdgeNode.Statement.User as User
import EdgeNode.Statement.Provider as Provider

import Auth
import KatipController
import Katip
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens
import Data.Aeson.Unit
import Data.Traversable
import Data.Int
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Word
import Data.Maybe

controller :: OnlyField "id" (Id "qualification") -> UserId -> KatipController (Response Unit)
controller qualification_id user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  resp <- katipTransactionViolationError hasql $ do
    deps_m <- statement Provider.getDepsQualifiationValues qualification_id
    compatilbity <- for deps_m $ \dep_xs -> do
       user_xs_m <- statement User.getUserQualificationValues user_id
       pure $ maybe 0.0 (calculateCompatibility dep_xs) user_xs_m
    statement User.addTrajectory (user_id, qualification_id, compatilbity)
  $(logTM) DebugS (logStr (show resp))
  let process (Right (Left e)) = Left e
      process (Right (Right _)) = Right Unit
      process (Left _) = Left User.AddTrajectoryErrorAlreadyAdded
  return $ (fromEither . process) resp


-- | Check calculateCompatibility
--
-- >>> :set -XOverloadedStrings
-- >>> :{
--     calculateCompatibility
--     [ (69, QualificationDegreeAdvancedLevelGCE,"A")
--     , (70, QualificationDegreeAdvancedLevelGCE, "B")
--     , (71, QualificationDegreeAdvancedLevelGCE, "B")
--     , (72, QualificationDegreeAdvancedLevelGCE, "B")]
--     [ (70, QualificationDegreeAdvancedLevelGCE, "C")
--     , (69, QualificationDegreeAdvancedLevelGCE, "C")
--     , (71, QualificationDegreeAdvancedLevelGCE, "B")
--     , (72, QualificationDegreeAdvancedLevelGCE, "A")]
-- :}
-- 90.0
--
-- >>> calculateCompatibility [(1, QualificationDegreeAdvancedLevelGCE,"D"), (2, QualificationDegreeToeflIBT, "40")] [(1, QualificationDegreeAdvancedLevelGCE, "B")]
-- 100.0
calculateCompatibility :: [(Int64, QualificationDegree, T.Text)] -> [(Int64, QualificationDegree, T.Text)] -> Double
calculateCompatibility qualification_xs user_xs =
  if (sum xs' / i) * 100 > 100
  then 100.0
  else (sum xs' / i) * 100
  where
    user_map = flip foldMap user_xs $ \(i, _, val) -> Map.insert i val $ mempty
    xs = [ (ty, val, (Map.!?) user_map i) | (i, ty, val) <- qualification_xs ]
    (xs', i) = foldr go ([], 0) xs
    go (ty, val_t, Nothing) tpl = tpl & _1 %~ (0:) & _2 %~ (+1)
    go (ty, val_t, Just val_t_u) tpl =
      let val = fromMaybe (mkError val_t) $ lookup val_t (qualificationDegreeToRange ty)
          val_u = fromMaybe (mkError val_t_u) $ lookup val_t_u (qualificationDegreeToRange ty)
      in tpl & _1 %~ ((fromIntegral val_u / fromIntegral val):) & _2 %~ (+1)
    mkError x = error $ "cannot find number for " <> show x

qualificationDegreeToRange :: QualificationDegree -> [(T.Text, Word32)]
qualificationDegreeToRange QualificationDegreeUnifiedStateExam = zip (map (T.pack . show) [1 .. 100 :: Word32]) [1 .. 100]
qualificationDegreeToRange QualificationDegreeAdvancedLevelGCE = zip ["E", "D", "C", "B", "A", "A*"] [1 ..]
qualificationDegreeToRange QualificationDegreeMagistr = zip ["Maj", "Std"] [1, 2]
qualificationDegreeToRange QualificationDegreeBakalavr = zip ["Maj", "Std"] [1, 2]
qualificationDegreeToRange QualificationDegreeBSc = zip ["1", "2:1", "2:2", "3"] [1 ..]
qualificationDegreeToRange QualificationDegreeBA = zip ["1", "2:1", "2:2", "3"] [1 ..]
qualificationDegreeToRange QualificationDegreeMSc = zip ["1", "2:1", "2:2", "3"] [1 ..]
qualificationDegreeToRange QualificationDegreeMA = zip ["1", "2:1", "2:2", "3"] [1 ..]
qualificationDegreeToRange QualificationDegreeMRes = zip ["1", "2:1", "2:2", "3"] [1 ..]
qualificationDegreeToRange QualificationDegreeToeflIBT = zip (map (T.pack . show) [0 .. 120 :: Int]) [1 ..]
qualificationDegreeToRange QualificationDegreeToeflPBT = zip (map (T.pack . show) [310 .. 677 :: Int]) [1 ..]
qualificationDegreeToRange QualificationDegreeTestDeConnaissanceDuFrançaisTCF = zip ["A1", "A2", "B1", "B2", "C1", "C2"] [1..]
qualificationDegreeToRange QualificationDegreeIELTS = zip (map (T.pack . show) [0, 0.5 .. 9 :: Double]) [1 ..]