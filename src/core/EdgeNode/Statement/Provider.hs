{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EdgeNode.Statement.Provider 
       ( getBranches
       , createBranches
       , checkHQ
       , BranchEncoder
       ) where

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
import qualified Data.Vector.Extended as V
import qualified Protobuf.Scalar as Protobuf
import Protobuf.Scalar (String (..))
import Database.Transaction
import TH.Mk
import EdgeNode.Country
import Proto3.Suite.Types
import qualified Data.Text.Lazy as LT
import Data.List
import Data.Generics.Product.Fields
import Data.DeriveTH
import Test.QuickCheck.Extended
import Prelude hiding (String)
import Data.Tuple.Ops

mkEncoder ''Branch
derive makeArbitrary ''String
derive makeArbitrary ''Branch

instance ParamsShow Branch where 
  render x = intercalate ", " $
    (mkEncoderBranch x
    & _1 %~ (^.lazytext.from stext)
    & _2 %~ (^.country.from stext)
    & _3 %~ (^._Just.field @"stringValue".lazytext.from stext)
    & _4 %~ (^._Just.field @"stringValue".lazytext.from stext)
    & _5 %~ (^._Just.field @"stringValue".lazytext.from stext)
    & _6 %~ (^._Just.field @"stringValue".lazytext.from stext)
    & _7 %~ (^._Just.field @"stringValue".lazytext.from stext))^..each

getBranches :: HS.Statement Id [WithId Id (OptField "files" [Id] (OptField "image" Id (WithField "isHQ" Bool Branch)))]
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
          branchDescription = x^?_9._Just.from lazytext.to Protobuf.String
      in WithField (x^._1.coerced) (AnyOptField (if Prelude.null files then Nothing else Just files) (AnyOptField image (WithField (x^._8) Branch {..})))

createBranches :: HS.Statement (Id, [OptField "files" [Id] (OptField "image" Id Branch)]) [Id]
createBranches = lmap mkEncoder $ statement $ premap (coerce @Int64 @Id) list
  where 
    statement = 
      [foldStatement|
        with
          providerfk as (
            select pu.provider_id as ident 
            from edgendode.provider_user as pu
            where pu.user_id = $1 :: int8),
          ids as (
            insert into edgenode.proivder_branch 
            (title, country, address, is_hq, provider_fk, 
             image_fk, additional_address, postcode, house, info)
            select x.t, x.c, x.a, false, 
                   (select ident from providerfk), 
                   x.ifk, x.ad, x.p, x.h, x.info
            from unnest(
              $3 :: text[], $4 :: text[], $5 :: text[], 
              $6 :: int8[], $7 :: text[], $8 :: text[], 
              $9 :: text[], $10 :: text[]) 
              as x(t, c, a, ifk, ad, p, h, info)
            returning id :: int8, $2 :: int8[] as xs)
          select id :: int8 from ids|]
    mkEncoder x = consT (coerce @_ @Int64 (x^._1)) (V.unzip9 $ V.fromList $ Prelude.map mkTpl (x^._2))
    mkTpl = undefined

checkHQ :: HS.Statement Id Bool
checkHQ = lmap (coerce @Id @Int64) statement
  where 
    statement = 
      [singletonStatement|
        select exists (
         select pb.id 
         from edgenode.provider_user as pu
         inner join edgenode.provider_branch as pb
         on pu.provider_id = pb.id
         where pu.user_id = $1 :: int8 and not pb.is_deleted and pb.is_hq) :: bool|]