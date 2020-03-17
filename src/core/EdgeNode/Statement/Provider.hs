{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module EdgeNode.Statement.Provider 
       ( getBranches
       , createBranches
       , checkHQ
       , createFiles
       , setHQ
       , updateBranches
       , publish
       , getQualificationBuilderBranches
       , saveQualification
       , getAreaToCountries
       , getCountryToTypes
       , getTypeToQualifications
       , saveDependencies
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Iso
import EdgeNode.Transport.Extended
import EdgeNode.Transport.Qualification
import EdgeNode.Transport.Validator (degreeToValues)

import TH.Proto
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
import Database.Transaction
import TH.Mk
import EdgeNode.Country
import Proto3.Suite.Types
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.List
import Data.Generics.Product.Fields
import Data.Tuple.Ops
import Data.Generics.Product.Positions
import Data.Time
import Data.Maybe
import Control.Lens.Each.Extended
import Data.Word
import Data.Aeson.WithField
import Data.Default.Class.Extended

deriving instance Enum QualificationDegree
deriving instance Enum Country
deriving instance Enum StudyTime
deriving instance Enum QualificationCategory
deriving instance Enum AcademicArea

mkEncoder ''Branch
mkArbitrary ''Branch
mkArbitrary ''Qualification
mkArbitrary ''QualificationBuilder
mkArbitrary ''TuitionFee
mkArbitrary ''Dependency
mkEncoder ''QualificationBuilder
mkEncoder ''Qualification
mkEncoder ''Dependency

instance Default Qualification

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
          branchPostcode = x^?_6._Just.from lazytext.to Protobuf.String
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

updateBranches :: HS.Statement [(Id "branch", Maybe (Id "img"), Branch)] ()
updateBranches = lmap mkEncoder statement
  where
    mkEncoder xs = 
      let tpl =  unzip3 xs
      in  consT ((V.fromList . coerce @_ @[Int64]) (tpl^._1)) $ 
          consT ((V.fromList . coerce @_ @[Maybe Int64]) (tpl^._2)) 
                ((V.unzip7 . V.fromList . Prelude.map mkTpl) (tpl^._3))
    mkTpl x = 
      ((mkEncoderBranch x) 
      & _1 %~ (^.lazytext)
      & _2 %~ (^.country)
      & _3 %~ (^?_Just.field @"stringValue".lazytext)
      & _4 %~ (^?_Just.field @"stringValue".lazytext)
      & _5 %~ (^?_Just.field @"stringValue".lazytext)
      & _6 %~ (^?_Just.field @"stringValue".lazytext)
      & _7 %~ (^?_Just.field @"stringValue".lazytext))
    statement =
      [resultlessStatement|
        update edgenode.provider_branch 
        set title = x.new_title,
            country = x.new_country,
            address = coalesce(x.new_address, address),
            image_fk = coalesce(x.new_image_fk, image_fk),
            additional_address = coalesce(x.new_additional_address, additional_address),
            postcode = coalesce(x.new_postcode, postcode),
            house = coalesce(x.new_house, house),
            info = coalesce(x.new_info, info),
            modified = now()
        from (
          select ident, new_image_fk, new_title, new_country, 
                 new_address, new_additional_address, new_postcode, new_house, new_info 
          from unnest($1 :: int8[], $2 :: int8?[], $3 :: text[], $4 :: text[], 
                      $5 :: text?[], $6 :: text?[], $7 :: text?[], $8 :: text?[], $9 :: text?[])
           as x(ident, new_image_fk, new_title, new_country, 
                new_address, new_additional_address, new_postcode, new_house, new_info)) as x
        where id = x.ident|]

publish :: HS.Statement (Id "user") ()
publish = 
  lmap (coerce @_ @Int64) 
  [resultlessStatement|
    insert into edgenode.provider_branch_public
    (title, country, address, image_fk, provider_branch_fk, provider_fk)
    select pb.title, pb.country, pb.address, pb.image_fk, pb.id, pb.provider_fk 
    from edgenode.provider_user as pu
    inner join edgenode.provider_branch as pb
    on pu.provider_id = pb.provider_fk
    where "user_id" = $1 :: int8
    on conflict (provider_branch_fk, provider_fk)
    do update 
    set title = excluded.title, country = excluded.country, 
    address = excluded.address, image_fk = excluded.image_fk, 
    provider_branch_fk = excluded.provider_branch_fk,
    provider_fk = excluded.provider_fk|]

getQualificationBuilderBranches :: HS.Statement (Id "user") [(Id "branch", T.Text)]
getQualificationBuilderBranches = 
  lmap (coerce @_ @Int64) $ 
  statement $ 
  premap (& _1 %~ (coerce @Int64 @(Id "branch"))) list
  where 
    statement = 
      [foldStatement|
        select pb.id :: int8, pb.title :: text
        from edgenode.provider_user as pu
        inner join edgenode.provider_branch as pb
        on pu.provider_id = pb.provider_fk
        where pu.user_id = $1 :: int8 order by pb.title asc|]

instance ParamsShow Qualification where 
  render x = intercalate ", " $
    (mkEncoderQualification x
     & _1 %~ (^.lazytext.from stext)
     & _2 %~ (\x -> "[" ++ intercalate ", " (x^..traversed.academicArea.from stext) ++ "]")
     & _3 %~ show
     & _4 %~ show
     & _5 %~ show
     & _6 %~ show
     & _7 %~ (^.qualCategory.from stext)
     & _8 %~ (^.qualStudyTime.from stext)
     & _9 %~ (^.qualQualificationDegree.from stext)
     & _10 %~ (maybe mempty (^.field @"stringValue".lazytext.from stext)))^..each
    
saveQualification  
  :: HS.Statement 
     (WithField 
      "branch" 
      (Id "branch") 
      Qualification) 
     (Id "qualification")
saveQualification = dimap mkEncoder (coerce @Int64 @(Id "qualification")) statement
  where 
    mkEncoder x =
      consT (coerce @_ @Int64 (x^.position @1))
      (mkEncoderQualification
       (x^.position @2)
       & _1 %~ (^.lazytext)
       & _2 %~ V.imap (\_ x -> (x^.academicArea))
       & _3 %~ fmap (fromIntegral @_ @Int64 . (^.field @"uint64Value"))
       & _4 %~ fmap (fromIntegral @_ @Int64 . (^.field @"uint64Value"))
       & _5 %~ fmap (^.field @"boolValue")
       & _6 %~ fmap (fromIntegral @_ @Int64 . (^.field @"uint64Value"))
       & _7 %~ (^.qualCategory)
       & _8 %~ (^.qualStudyTime)
       & _9 %~ (^.qualQualificationDegree)
       & _10 %~ (^?_Just.field @"stringValue".lazytext))
    statement = 
      [singletonStatement|
        insert into edgenode.provider_branch_qualification
        (title, academic_area, start, finish, 
         is_repeated, application_deadline, category,
         study_time, type, created, provider_branch_fk, 
         min_degree_value) values 
        ($2 :: text, $3 :: text[], to_timestamp($4 :: int8?), 
         to_timestamp($5 :: int8?), $6 :: bool?,
         to_timestamp($7 :: int8?), $8 :: text, $9 :: text, 
         $10 :: text, now(), $1 :: int8, $11 :: text?) returning id :: int8|]

instance ParamsShow EdgeNodeAcademicArea where
  render x = x^.isoEdgeNodeAcademicArea

getAreaToCountries :: HS.Statement EdgeNodeAcademicArea [EdgeNodeCountry]
getAreaToCountries = 
  lmap (^.isoEdgeNodeAcademicArea.stext) $
  [foldStatement|
    select distinct on (pb.country) pb.country :: text 
    from edgenode.provider_branch as pb 
    left join edgenode.provider_branch_qualification as pbq 
    on pb.id = pbq.provider_branch_fk
    where array[$1 :: text] :: text[] <@ pbq.academic_area|] $ 
  premap (^.from stext.from isoEdgeNodeCountry) list

instance ParamsShow EdgeNodeCountry where
  render x = x^.isoEdgeNodeCountry

getCountryToTypes :: HS.Statement (EdgeNodeAcademicArea, EdgeNodeCountry) [EdgeNodeQualificationDegree]
getCountryToTypes =
  lmap (\x -> 
    x & _1 %~ (^.isoEdgeNodeAcademicArea.stext)
      & _2 %~  (^.isoEdgeNodeCountry.stext)) $
  [foldStatement|
    select distinct on (pbq.type) pbq.type :: text
    from edgenode.provider_branch as pb 
    left join edgenode.provider_branch_qualification as pbq 
    on pb.id = pbq.provider_branch_fk
    where array[$1 :: text] :: text[] <@ pbq.academic_area
          and pb.country = $2 :: text|] $
  premap (^.from stext.from isoEdgeNodeQualificationDegree) list

instance ParamsShow EdgeNodeQualificationDegree where
  render x = x^.isoEdgeNodeQualificationDegree

getTypeToQualifications 
  :: HS.Statement 
     (EdgeNodeAcademicArea, 
      EdgeNodeCountry, 
      EdgeNodeQualificationDegree) 
     [WithId (Id "qualification") DegreeTypeToQualification]
getTypeToQualifications =
  lmap (\x -> 
    x & _1 %~ (^.isoEdgeNodeAcademicArea.stext)
      & _2 %~  (^.isoEdgeNodeCountry.stext)
      & _3 %~  (^.isoEdgeNodeQualificationDegree.stext)) $
  [foldStatement|
    select pbq.id :: int8, pbq.title :: text, 
    pbq.min_degree_value :: text?, pbq.type :: text 
    from edgenode.provider_branch as pb 
    left join edgenode.provider_branch_qualification as pbq 
    on pb.id = pbq.provider_branch_fk
    where array[$1 :: text] :: text[] <@ pbq.academic_area
          and pb.country = $2 :: text and pbq.type = $3 :: text|] $
    premap mkDecoder list
  where
    mkDecoder x = 
      let vs = 
            fromJust $ 
            Data.List.lookup 
            (x^._4.from stext.from isoQualificationDegree) 
            degreeToValues
      in WithField (coerce (x^._1)) $ 
          DegreeTypeToQualification 
          (x^._2.from lazytext) 
          (V.fromList $ 
           maybe vs
           (\v -> dropWhile (/= v) vs) 
           (x^?_3._Just.from lazytext))

instance ParamsShow Dependency where
  render x = intercalate ", " $
    (mkEncoderDependency x
    & _1 %~ show 
    & _2 %~ show 
    & _3 %~ (^.lazytext.from stext))^..each

saveDependencies :: HS.Statement (Id "qualification", V.Vector Dependency) ()
saveDependencies = 
  lmap mkEncoder 
  [resultlessStatement|
    insert into edgenode.provider_branch_qualification_dependency 
    (cluster, required_degree, 
     provider_branch_qualification_fk, dependency_fk) 
    select cluster, req_degree, qual_fk, $1 :: int8 
    from unnest($2 :: int4[], $4 :: text[], $3 :: int8[]) 
    as x(cluster, req_degree, qual_fk)|]
  where 
    mkEncoder (ident, v) = consT (coerce ident) (V.unzip3 (V.map mkTpl v))
    mkTpl x = (mkEncoderDependency x) & _1 %~ fromIntegral & _2 %~ fromIntegral & _3 %~ (^.lazytext)