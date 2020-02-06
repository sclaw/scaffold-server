{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Statement.Provider (getBranches) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Iso

import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Aeson.WithField.Extended
import Data.Coerce
import Control.Lens
import Data.Int
import Control.Lens.Iso.Extended
import Control.Foldl
import qualified Data.Vector as V
import qualified Protobuf.Scalar as Protobuf 

getBranches :: HS.Statement Id [WithId Id (OptField "files" [Id] (OptField "image" Id Branch))]
getBranches = lmap (coerce @_ @Int64) $ statement $ premap mkBranch list 
  where
    statement =
     [foldStatement|
       select 
         pb.id :: int8, pb.title :: text, pb.country :: text, 
         pb.address :: text?, pb.additional_address :: text?, 
         pb.postcode :: text?, pb.house :: text?, pb.is_hq :: bool, pb.info :: text?,
         pb.image_fk :: int8?, array_agg(pbf.file_fk) :: int8[]
       from auth.user as au
       inner join edgenode.provider_user as pu
       on au.id = pu.user_id
       inner join edgenode.provider_branch as pb
       on pb.provider_fk = pu.provider_id
       left join edgenode.provider_branch_file as pbf
       on pbf.provider_branch_fk = pb.id
       where au.id = $1 :: int8 and not pb.is_deleted
       group by pb.id|]
    mkBranch x =
      let files = (V.toList (x^._11))^..traversed.coerced @_ @_ @Id @_
          image = x^?_10._Just.coerced
          branchTitle    = x^._2.from lazytext    
          branchCountry  = x^._3.from country
          branchAddress1 = x^?_4._Just.from lazytext.to Protobuf.String
          branchAddress2 = x^?_5._Just.from lazytext.to Protobuf.String
          branchPostode  = x^?_6._Just.from lazytext.to Protobuf.String
          branchHouse    = x^?_7._Just.from lazytext.to Protobuf.String
          branchIsHQ     = x^._8
          branchInfo = x^?_9._Just.from lazytext.to Protobuf.String
      in WithField (x^._1.coerced) (AnyOptField (if Prelude.null files then Nothing else Just files) (AnyOptField image Branch {..}))