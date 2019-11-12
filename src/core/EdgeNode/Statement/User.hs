{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Statement.User 
       (deleteQualification
       , deleteTrajectory
       ) where

import EdgeNode.Model.User
import EdgeNode.Model.Qualification
import EdgeNode.Model.User.Qualification
import EdgeNode.Model.User.Trajectory

import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.String.Interpolate
import Control.Lens
import Control.Monad

deleteQualification :: HS.Statement (UserQualificationId, UserId) [QualificationId]  
deleteQualification = HS.Statement sql encoder decoder False
  where 
    sql = 
      [i|with del as 
         (delete from "edgeNode"."UserQualification" 
            where id = $1 and "userId" = $2
            returning "qualificationKey" as k)
         select coalesce(array_agg("key"), array[]::int8[])
         from "edgeNode"."Trajectory" as tr
         left join "edgeNode"."QualificationDependency" as qd
         on qd."key" = tr."qualificationKey" 
         where tr."user" = $2 and qd."dependency" = (select k from del)|]
    encoder =
      contramap (^._1._Wrapped') (HE.param HE.int8) <>
      contramap (^._1._Wrapped') (HE.param HE.int8)
    decoder = HD.singleRow $ HD.column $ HD.array (HD.dimension replicateM (HD.element (HD.int8 <&> (^.from _Wrapped'))))

deleteTrajectory :: HS.Statement (TrajectoryId, UserId) ()
deleteTrajectory = HS.Statement sql encoder HD.unit False
  where
    sql = [i|delete from "edgeNode"."Trajectory" where "user" = $2 and id = $1|]
    encoder =
      contramap (^._1._Wrapped') (HE.param HE.int8) <>
      contramap (^._1._Wrapped') (HE.param HE.int8)