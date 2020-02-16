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
       , createFiles
       , setHQ
       , BranchEncoder
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Iso
import EdgeNode.Transport.ProviderExt (GetBranchResp (..))

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
import Data.Generics.Product.Positions

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

getBranches :: HS.Statement (Id "user") [GetBranchResp]
getBranches = lmap (coerce @_ @Int64) $ statement $ premap mkBranch list 
  where
    statement =
     [foldStatement|
       select 
         pb.id :: int8, pb.title :: text, pb.country :: text, 
         pb.address :: text?, pb.additional_address :: text?, 
         pb.postcode :: text?, pb.house :: text?, pb.is_hq :: bool, pb.info :: text?,
         pb.image_fk :: int8? , array_agg(pbf.file_fk) filter (where pbf.file_fk is not null) :: int8[]?
       from auth.user as au
       inner join edgenode.provider_user as pu
       on au.id = pu.user_id
       inner join edgenode.provider_branch as pb
       on pb.provider_fk = pu.provider_id
       left join edgenode.provider_branch_file as pbf
       on pbf.provider_branch_fk = pb.id
       where au.id = $1 :: int8 and not pb.is_deleted
       group by pb.id
       order by pb.is_hq desc, pb.title|]  
    mkBranch x =
      let files = flip fmap (x^?_11._Just) $ \x -> V.toList x^..traversed.coerced @_ @_ @(Id "file") @_
          image = x^?_10._Just.coerced
          branchTitle    = x^._2.from lazytext
          branchCountry  = x^._3.from country
          branchAddress1 = x^?_4._Just.from lazytext.to Protobuf.String
          branchAddress2 = x^?_5._Just.from lazytext.to Protobuf.String
          branchPostode  = x^?_6._Just.from lazytext.to Protobuf.String
          branchHouse    = x^?_7._Just.from lazytext.to Protobuf.String
          branchDescription = x^?_9._Just.from lazytext.to Protobuf.String
      in GetBranchResp (x^._1.coerced) files image (x^._8) Branch {..}

createBranches :: HS.Statement (Id "user", [OptField "image" (Id "img") Branch]) [Id "branch"]
createBranches = lmap mkEncoder $ statement $ premap (coerce @Int64 @(Id "branch")) list
  where 
    statement = 
      [foldStatement|
        with
          providerfk as (
            select pu.provider_id as ident 
            from edgenode.provider_user as pu
            where pu.user_id = $1 :: int8)  
        insert into edgenode.provider_branch 
        (image_fk, title, country, address, is_hq, provider_fk, 
         additional_address, postcode, house, info)
        select x.ifk, x.t, x.c, x.a, false as hq, 
              (select ident from providerfk) as ident, 
              x.ad, x.p, x.h, x.info
        from unnest(
         $2 :: int8?[], $3 :: text[], $4 :: text[], 
         $5 :: text[], $6 :: text[], $7 :: text[], 
         $8 :: text[], $9 :: text[])
         as x(ifk, t, c, a, ad, p, h, info)
        returning id :: int8 as ident|]
    mkEncoder x = consT (coerce @_ @Int64 (x^._1)) (V.unzip8 (V.fromList $ Prelude.map mkTpl (x^._2))) 
    mkTpl x =  
      consT (fmap (coerce @_ @Int64) (x^.position @1.position @1)) $
      ((mkEncoderBranch (x^.position @1.position @2)) 
       & _1 %~ (^.lazytext)
       & _2 %~ (^.country)
       & _3 %~ (^._Just.field @"stringValue".lazytext)
       & _4 %~ (^._Just.field @"stringValue".lazytext)
       & _5 %~ (^._Just.field @"stringValue".lazytext)
       & _6 %~ (^._Just.field @"stringValue".lazytext)
       & _7 %~ (^._Just.field @"stringValue".lazytext))

checkHQ :: HS.Statement (Id "branch") Bool
checkHQ = lmap (coerce @(Id "branch") @Int64) statement
  where 
    statement = 
      [singletonStatement|
        select exists (
         select pb.id 
         from edgenode.provider_user as pu
         inner join edgenode.provider_branch as pb
         on pu.provider_id = pb.id
         where pu.user_id = $1 :: int8 and not pb.is_deleted and pb.is_hq) :: bool|]

createFiles :: HS.Statement [(Id "branch" , Id "file")] ()
createFiles = lmap mkEncoder statement
  where
    statement = 
      [resultlessStatement|
        insert into edgenode.provider_branch_file
        (provider_branch_fk, file_fk)
        select p, f from unnest($1 :: int8[], $2 :: int8[]) as x(p, f)|]
    mkEncoder = V.unzip. V.fromList . Prelude.map (coerce @(Id "branch", Id "file") @(Int64, Int64))

setHQ :: HS.Statement (Id "user", Id "branch") Bool
setHQ = dimap coerce (> 0) statement
  where
    statement = 
      [rowsAffectedStatement|
        with setoffhq as (
         update edgenode.provider_branch 
         set is_hq = false
         where provider_fk in 
               (select provider_id 
                from edgenode.provider_user 
                where "user_id" = $1::int8) and is_hq)
        update edgenode.provider_branch 
         set is_hq = true, modified = now()
         where id = $2 :: int8 and 
               provider_fk in 
               (select provider_id 
                from edgenode.provider_user 
                where "user_id" = $1::int8)|]