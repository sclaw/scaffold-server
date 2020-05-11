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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
       , saveTuitionFees
       , getQualifications
       , getQualificationById
       , patchQualification
       , patchTuitionFees
       , patchClusters
       , deleteFees
       , deleteClusters
       , deleteDeps
       , getDepsQualifiationValues
       ) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Iso
import EdgeNode.Transport.Extended
import EdgeNode.Transport.Qualification
import EdgeNode.Transport.Validator (degreeToValues)
import qualified EdgeNode.Transport.Error as Error

import KatipController
import Katip
import Pretty
import Auth
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
import Data.Generics.Product.Positions
import Data.Time
import Data.Maybe
import Control.Lens.Each.Extended
import Data.Word
import Data.Aeson.WithField
import Data.Default.Class.Extended
import Data.Aeson
import GHC.Generics hiding (from, to)
import qualified Data.Vector.Lens as L.V
import System.IO.Unsafe
import qualified Text.RE.Replace as Regexp
import qualified Text.RE.PCRE as Regexp
import Data.Char
import Data.Tuple.Extended
import Data.Traversable
import Data.String.Conv

deriving instance Enum QualificationDegree
deriving instance Enum Country
deriving instance Enum StudyTime
deriving instance Enum AcademicArea
deriving instance Enum Currency
deriving instance Enum Period

mkEncoder ''Branch
mkArbitrary ''Branch
mkArbitrary ''Qualification
mkArbitrary ''Cluster
mkArbitrary ''QualificationBuilder
mkArbitrary ''TuitionFees
mkArbitrary ''Dependency
mkArbitrary ''EdgeNodeQualificationDegree
mkEncoder ''QualificationBuilder
mkEncoder ''Qualification
mkEncoder ''Dependency
mkEncoder ''TuitionFees
mkEncoder ''PatchQualificationItem
mkArbitrary ''PatchDegreeValue
mkArbitrary ''PatchQualificationDegree
mkArbitrary ''PatchStudyTime
mkArbitrary ''PatchApplicationDeadline
mkArbitrary ''PatchIsRepeated
mkArbitrary ''PatchFinish
mkArbitrary ''PatchStart
mkArbitrary ''PatchAcademicArea
mkArbitrary ''PatchTitle
mkArbitrary ''PatchBranch
mkArbitrary ''PatchQualificationItem
mkEncoder ''PatchTuitionFees_Fees
mkEncoder ''FeesInfo
mkArbitrary ''FeesInfo
mkArbitrary ''PatchTuitionFees_Fees

instance Default AcademicArea where def = toEnum 0
instance Default StudyTime where def = toEnum 0
instance Default Country where def = toEnum 0
instance Default QualificationDegree where def = toEnum 0

instance Default Qualification
instance Default ClusterInfo
instance Default ClusterInfo_Dependency
instance Default QualificationInfo_Cluster
instance Default DependencyInfo
instance Default QualificationInfo_Fees
instance Default QualificationInfo

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
     & _7 %~ (^.qualStudyTime.from stext)
     & _8 %~ (^.qualQualificationDegree.from stext)
     & _9 %~ (maybe mempty (^.field @"stringValue".lazytext.from stext)))^..each

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
       & _2 %~ (toJSON . V.map (^.academicArea))
       & _3 %~ fmap (fromIntegral @_ @Int64 . (^.field @"uint64Value"))
       & _4 %~ fmap (fromIntegral @_ @Int64 . (^.field @"uint64Value"))
       & _5 %~ fmap (^.field @"boolValue")
       & _6 %~ fmap (fromIntegral @_ @Int64 . (^.field @"uint64Value"))
       & _7 %~ (^.qualStudyTime)
       & _8 %~ (^.qualQualificationDegree)
       & _9 %~ (^?_Just.field @"stringValue".lazytext))
    statement =
      [singletonStatement|
        insert into edgenode.provider_branch_qualification
        (title, academic_area, start, finish,
         is_repeated, application_deadline,
         study_time, type, created, provider_branch_fk,
         min_degree_value) values
        ($2 :: text, $3 :: jsonb, to_timestamp($4 :: int8?),
         to_timestamp($5 :: int8?), $6 :: bool?,
         to_timestamp($7 :: int8?), $8 :: text,
         $9 :: text, now(), $1 :: int8, $10 :: text?) returning id :: int8|]

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
    where $1 :: text in (select * from jsonb_array_elements_text(pbq.academic_area))|] $
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
    where $1 :: text in
          (select * from
           jsonb_array_elements_text
           (pbq.academic_area))
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
    where $1 :: text in
          (select * from
           jsonb_array_elements_text
           (pbq.academic_area))
          and pb.country = $2 :: text
          and pbq.type = $3 :: text|] $
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
    & _1 %~ (^.academicArea.from stext)
    & _2 %~ show
    & _3 %~ (^.lazytext.from stext))^..each

instance ParamsShow Cluster where
  render (Cluster v) = intercalate ", " $ V.foldl (\xs e -> xs ++ ["[" ++ render e ++ "]"]) [] v

saveDependencies :: HS.Statement (Id "qualification", V.Vector Cluster) ()
saveDependencies =
  lmap mkEncoder
  [resultlessStatement|
    insert into edgenode.provider_branch_qualification_dependency
    (cluster, position, academic_area,
     provider_branch_qualification_fk,
     dependency_fk, required_degree)
    select cluster, pos, ac_area, qual_fk, $1 :: int8, req_degree
    from unnest($2 :: int4[], $3 :: int4[], $4 :: text[], $5 :: int8[], $6 :: text[])
    as x(cluster, pos, ac_area, qual_fk, req_degree)|]
  where
    mkEncoder (ident, v) = consT (coerce ident) (V.unzip5 (V.concat (V.foldl go [] (V.indexed v))))
    go xs (idx, Cluster v) = xs ++ [V.map (mkTpl idx) (V.indexed v)]
    mkTpl idx (pos, x) =
      consT (fromIntegral idx) $
      consT (fromIntegral pos) $
      ((mkEncoderDependency x) & _1 %~ (^.academicArea)  & _2 %~ fromIntegral & _3 %~ (^.lazytext))

instance ParamsShow TuitionFees where
  render x = intercalate ", " $
    (mkEncoderTuitionFees x
     & _1 %~ show
     & _2 %~ (^.currency.from stext)
     & _3 %~ (^.period.from stext)
     & _4 %~ (\xs -> "[ " ++ intercalate ", " (Prelude.foldMap ((:[]) . (^.country.from stext)) xs) ++ "]"))^..each

saveTuitionFees :: HS.Statement (Id "qualification", V.Vector TuitionFees) ()
saveTuitionFees =
  lmap mkEncoder
  [resultlessStatement|
    insert into edgenode.provider_branch_qualification_tuition_fees
    (position, amount, currency, period,
     provider_branch_qualification_fk, countries)
    select pos, am, curr, per, $1 :: int8, cs
    from unnest(
      $2 :: int4[], $3 :: float8[], $4 :: text[],
      $5 :: text[], $6 :: jsonb[])
    as x(pos, am, curr, per, cs)|]
  where
    mkEncoder (ident, v) = consT (coerce ident) (V.unzip5 (V.map go (V.indexed v)))
    go (pos, x) =
      consT (fromIntegral pos) $
      (mkEncoderTuitionFees x
       & _2 %~ (^.currency)
       & _3 %~ (^.period)
       & _4 %~ (toJSON . (V.map (^.country))))

getQualifications :: HS.Statement (Id "user") [WithId (Id "qualification") ListItem]
getQualifications = lmap (coerce @_ @Int64) $ statement $ premap mkItem list
  where
    statement =
      [foldStatement|
        select
          pbq.id :: int8, pbq.title :: text,
          pbq.type :: text, pb.title :: text
        from edgenode.provider_user as pu
        inner join edgenode.provider as p
        on pu.provider_id = p.id
        left join edgenode.provider_branch as pb
        on p.id = pb.provider_fk
        left join edgenode.provider_branch_qualification as pbq
        on pb.id = pbq.provider_branch_fk
        where pu.user_id = $1 :: int8
              and pbq.id is not null
              and not pbq.is_deleted
        order by pb.title, pbq.title|]
    mkItem x = WithField (x^._1.coerced) $ ListItem (x^._2.from lazytext) (x^._3.from qualQualificationDegree) (x^._4.from lazytext)

data GetQualificationByIdError
     = GetQualificationByIdNF
     | GetQualificationByIdJsonDecodeError String
     deriving stock Show

instance Error.AsError GetQualificationByIdError where
  asError GetQualificationByIdNF = Error.asError @T.Text "qualification not found"
  asError (GetQualificationByIdJsonDecodeError _) = Error.asError @T.Text "json decode error"

newtype QICW = QICW QualificationInfo_Cluster

instance FromJSON QICW where
  parseJSON = withObject "QICW" $ \object_raw -> do
    let result_e =
          fromJSON $
          read $
          replaceEnumQICW $
          show $
          Object object_raw
    case result_e of
      Success x -> pure $ QICW x
      Error e -> error e

newtype QIFW = QIFW QualificationInfo_Fees

instance FromJSON QIFW where
  parseJSON = withObject "QIFW" $ \object_raw -> do
    let result_e =
          fromJSON $
          read $
          replaceEnumQIFW $
          show $
          Object object_raw
    case result_e of
      Success x -> pure $ QIFW x
      Error e -> error e

replaceEnumQICW :: String -> String
replaceEnumQICW =
 go degreeTypePattern
 (maybe mempty ((\s -> "\"" ++ s ++ "\"")  . T.unpack) .
  T.stripPrefix "QualificationDegree" .
  T.pack .
  show .
  toQualificationDegree) .
 go academicAreaPattern
 (maybe mempty ((\s -> "\"" ++ s ++ "\"") . T.unpack) .
  T.stripPrefix "AcademicArea" .
  T.pack .
  show .
  toAcademicArea) .
 go countryPattern
 (maybe mempty ((\s -> "\"" ++ s ++ "\"") . T.unpack) .
  T.stripPrefix "Country" .
  T.pack .
  show .
  toCountry)
  where
    go pattern transform src =
      Regexp.replaceAllCaptures
      Regexp.ALL
      (replace transform) $
      src Regexp.*=~ pattern
    replace transform _ loc cap = Just $
      case Regexp.locationCapture loc of
        0 -> transform $ init $ tail $ Regexp.capturedText cap
        _ -> error "replaceEnumQICW:go"
    countryPattern = [Regexp.re|"(?<=\"country\",String\s\")[a-z_]*(?=\"\))"|]
    academicAreaPattern = [Regexp.re|"(?<=\"academicArea\",String\s\")[a-z_]*(?=\"\))"|]
    degreeTypePattern = [Regexp.re|"(?<=\"degreeType\",String\s\")[a-z_]*(?=\"\))"|]

replaceEnumQIFW :: String -> String
replaceEnumQIFW =
 go currencyPattern
 (maybe mempty ((\s -> "\"" ++ s ++ "\"") . T.unpack) .
  T.stripPrefix "Currency" .
  T.pack .
  show .
  toCurrency) .
 go periodPattern
 (maybe mempty ((\s -> "\"" ++ s ++ "\"") . T.unpack) .
  T.stripPrefix "Period" .
  T.pack .
  show .
  toPeriod) .
 goC countriesPattern
 (T.unpack .
  fromMaybe  mempty .
  T.stripPrefix "Country" .
  T.pack .
  show .
  toCountry)
  where
    go pattern transform src =
      Regexp.replaceAllCaptures
      Regexp.ALL
      (replace transform) $
      src Regexp.*=~ pattern
    replace transform _ loc cap = Just $
      case Regexp.locationCapture loc of
        0 -> transform $ init $ tail $ Regexp.capturedText cap
        _ -> error "replaceEnumQIFW:go"
    goC pattern transform src =
      Regexp.replaceAllCaptures
      Regexp.ALL
      (replaceC transform) $
      src Regexp.*=~ pattern
    replaceC transform _ loc cap = Just $
      case Regexp.locationCapture loc of
        0 -> transform $ Regexp.capturedText cap
        _ -> error "replaceEnumQIFW:go"
    currencyPattern = [Regexp.re|"(?<=\"currency\",String\s\")[a-z_]*(?=\"\))"|]
    periodPattern = [Regexp.re|"(?<=\"period\",String\s\")[a-z_]*(?=\"\))"|]
    countriesPattern = [Regexp.re|(?<=Array\s\[String\s\"|,String\s\")[a-z_]*(?=\",|\"\])|]

getQualificationById :: HS.Statement (UserId, Id "qualification") (Either GetQualificationByIdError QualificationInfo)
getQualificationById =
  dimap (bimap (coerce @(Id "user") @Int64) (coerce @_ @Int64))
        (maybe (Left GetQualificationByIdNF) mkResp . fmap mkInfo) $
  statement
  where
    statement =
      [maybeStatement|
        with
          clusters as (
            select
              pbqd.cluster,
              pbqd.dependency_fk as ident,
              array_agg(jsonb_build_object(
              'position', pbqd.position,
              'ident', pbqd.id,
              'value', jsonb_build_object(
               'academicArea', pbqd.academic_area,
               'country', pb.country,
               'degreeType', pbq.type,
               'qualification', jsonb_build_object(
                 'ident', pbq.id,
                 'title', pbq.title),
               'requiredDegree', pbqd.required_degree))
                order by pbqd.position) as dependencies
            from edgenode.provider_branch_qualification_dependency as pbqd
            inner join edgenode.provider_branch_qualification as pbq
            on pbqd.provider_branch_qualification_fk = pbq.id
            inner join edgenode.provider_branch as pb
            on pbq.provider_branch_fk = pb.id
            group by pbqd.cluster, pbqd.dependency_fk),
          fees as (
            select
              pbq.id as ident,
              array_agg(jsonb_build_object(
                'position', pbqtf.position,
                'ident', pbqtf.id,
                'value', jsonb_build_object(
                  'price', pbqtf.amount,
                  'currency', pbqtf.currency,
                  'period', pbqtf.period,
                  'countries', pbqtf.countries
                )
              )) as fs
            from edgenode.provider_branch_qualification_tuition_fees as pbqtf
            inner join edgenode.provider_branch_qualification as pbq
            on pbqtf.provider_branch_qualification_fk = pbq.id
            group by pbq.id)
        select
          pbq.title :: text,
          pbq.academic_area :: jsonb,
          date_part('epoch', pbq.start) :: int8?,
          date_part('epoch', pbq.finish) :: int8?,
          pbq.is_repeated :: bool?,
          date_part('epoch', pbq.application_deadline) :: int8?,
          pbq.study_time :: text,
          pbq.type :: text,
          pbq.min_degree_value :: text?,
          array_agg(jsonb_build_object(
            'cluster', c.cluster,
            'value', jsonb_build_object('dependencies', c.dependencies)
          )) :: jsonb[],
        pb.id :: int8,
        pb.title :: text,
        f.fs :: jsonb[]?
        from edgenode.provider_user as pu
        inner join edgenode.provider as p
        on pu.provider_id = p.id
        left join edgenode.provider_branch as pb
        on p.id = pb.provider_fk
        left join edgenode.provider_branch_qualification as pbq
        on pb.id = pbq.provider_branch_fk
        left join clusters as c
        on pbq.id = c.ident
        left join fees as f
        on pbq.id = f.ident
        where pu.user_id = $1 :: int8 and pbq.id = $2 :: int8 and not pbq.is_deleted
        group by pbq.title, pbq.academic_area, pbq.start,
        finish, pbq.is_repeated, pbq.application_deadline,
        pbq.study_time, pbq.type, pbq.min_degree_value, pb.id, pb.title, f.fs|]
    mkInfo x =
      let json_result = do
                 areas <- fromJSON @[T.Text] (x^._2)
                 clusters <- traverse (coerce . fromJSON @QICW) (x^._10)
                 fees <- for (x^._13) $ traverse (coerce . fromJSON @QIFW)
                 pure (areas, clusters, fees)
      in flip fmap json_result $ \(areas, clusters, fees) ->
          (def :: QualificationInfo)
          & field @"qualificationInfoQualification" ?~
            ((def :: Qualification)
            & field @"qualificationTitle" .~ (x^._1.from lazytext)
            & field @"qualificationAreas" .~ (areas^..traversed.from academicArea)^.L.V.vector
            & field @"qualificationStart" .~ fmap (Protobuf.UInt64 . fromIntegral) (x^._3)
            & field @"qualificationFinish" .~ fmap (Protobuf.UInt64 . fromIntegral) (x^._4)
            & field @"qualificationIsRepeated" .~ fmap Protobuf.Bool (x^._5)
            & field @"qualificationApplicationDeadline" .~ fmap (Protobuf.UInt64 . fromIntegral) (x^._6)
            & field @"qualificationStudyTime" .~ x^._7.from qualStudyTime
            & field @"qualificationDegreeType" .~ x^._8.from qualQualificationDegree
            & field @"qualificationDegreeValue" .~ x^?_9._Just.from lazytext.to Protobuf.String)
          & field @"qualificationInfoClusters" .~ clusters
          & field @"qualificationInfoFees" .~ fromMaybe mempty fees
          & field @"qualificationInfoBranch" ?~ BranchInfo (x^._11.integral) (x^._12.from lazytext)
    mkResp (Error error) = Left (GetQualificationByIdJsonDecodeError error)
    mkResp (Success info) = Right info

instance ParamsShow PatchQualificationItem where
  render x = intercalate ", " $
    (mkEncoderPatchQualificationItem x
    & _1 %~ (fromMaybe mempty . fmap (^.field @"patchTitleValue".lazytext.from stext))
    & _2 %~ (fromMaybe mempty . fmap (^.field @"patchAcademicAreaValue".from L.V.vector.traversed.to (^.academicArea.from stext)))
    & _3 %~ (fromMaybe mempty . fmap (^.field @"patchStartValue".to show))
    & _4 %~ (fromMaybe mempty . fmap (^.field @"patchFinishValue".to show))
    & _5 %~ (fromMaybe mempty . fmap (^.field @"patchIsRepeatedValue".to show))
    & _6 %~ (fromMaybe mempty . fmap (^.field @"patchApplicationDeadlineValue".to show))
    & _7 %~ (fromMaybe mempty . fmap (^.field @"patchStudyTimeValue".qualStudyTime.from stext))
    & _8 %~ (fromMaybe mempty . fmap (^.field @"patchQualificationDegreeValue".qualQualificationDegree.from stext))
    & _9 %~ (fromMaybe mempty . fmap (^.field @"patchDegreeValueValue".lazytext.from stext))
    & _10 %~ (fromMaybe mempty . fmap (^.field @"patchBranchValue".to show))) ^..each

patchQualification :: HS.Statement (Id "qualification", UserId, PatchQualificationItem) ()
patchQualification = lmap mkEncoder statement
  where
    mkEncoder (qual_id, user_id, patch) =
      consT (coerce @_ @Int64 qual_id) $
        consT (coerce @_ @Int64 user_id)
        (mkEncoderPatchQualificationItem patch
         & _1 %~ fmap (^.field @"patchTitleValue".lazytext)
         & _2 %~ fmap (toJSON . (^..field @"patchAcademicAreaValue".from L.V.vector.traversed.to (^.academicArea)))
         & _3 %~ fmap (^.field @"patchStartValue".integral)
         & _4 %~ fmap (^.field @"patchFinishValue".integral)
         & _5 %~ fmap (^.field @"patchIsRepeatedValue")
         & _6 %~ fmap (^.field @"patchApplicationDeadlineValue".integral)
         & _7 %~ fmap (^.field @"patchStudyTimeValue".qualStudyTime)
         & _8 %~ fmap (^.field @"patchQualificationDegreeValue".qualQualificationDegree)
         & _9 %~ fmap (^.field @"patchDegreeValueValue".lazytext)
         & _10 %~ fmap (^.field @"patchBranchValue".integral))
    statement =
      [resultlessStatement|
        update edgenode.provider_branch_qualification set
        title = coalesce($3 :: text?, title),
        academic_area = coalesce($4 :: jsonb?, academic_area),
        start = coalesce(to_timestamp($5 :: int8?), start),
        finish = coalesce(to_timestamp($6 :: int8?), finish),
        is_repeated = coalesce($7 :: bool?, is_repeated),
        application_deadline = coalesce(to_timestamp($8 :: int8?), application_deadline),
        study_time = coalesce($9 :: text?, study_time),
        type= coalesce($10 :: text?, type),
        min_degree_value = coalesce($11 :: text?, min_degree_value),
        provider_branch_fk = coalesce($12 :: int8?, provider_branch_fk),
        modified = now()
        where "id" =
         (select pbq.id from edgenode.provider_user as pu
          inner join edgenode.provider as pv
          on pu.provider_id = pv.id
          left join edgenode.provider_branch as pb
          on pb.provider_fk = pv.id
          left join edgenode.provider_branch_qualification as pbq
          on pbq.provider_branch_fk = pb.id
          where pu.user_id = $2 :: int8 and pbq.id = $1 :: int8 and not pbq.is_deleted)|]

patchClusters :: HS.Statement (Id "qualification", V.Vector (Int32, Int32, Dependency)) ()
patchClusters = lmap mkTpl statement
  where
    mkTpl (i, xs) =
      consT (coerce i) $
      V.unzip5 $
      xs <&> \(c, p, dep) ->
        consT c $
        consT p $
        ((mkEncoderDependency dep)
          & _1 %~ (^.academicArea)
          & _2 %~ fromIntegral
          & _3 %~ (^.lazytext))
    statement =
      [resultlessStatement|
        insert into edgenode.provider_branch_qualification_dependency
        (provider_branch_qualification_fk, cluster, position, academic_area, dependency_fk, required_degree)
        select dep, cl, pos, area, $1 :: int8, val
        from unnest($2 :: int4[], $3 :: int4[], $4 :: text[], $5 :: int8[], $6 :: text[])
        as x(cl, pos, area, dep, val)
        on conflict (cluster, provider_branch_qualification_fk, dependency_fk)
        do update set required_degree = excluded.required_degree, modified = now()|]

instance ParamsShow PatchTuitionFees_Fees where
  render fees =
    maybe mempty (intercalate ", ") $
    flip fmap fees_info_m $ \x ->
      (mkEncoderFeesInfo x
       & _1 %~ show
       & _2 %~ (^.currency.from stext)
       & _3 %~ (^.period.from stext)
       & _4 %~ (\xs -> "[ " ++ intercalate ", " (Prelude.foldMap ((:[]) . (^.country.from stext)) xs) ++ "]"))^..each
    where (_, fees_info_m) = mkEncoderPatchTuitionFees_Fees fees

patchTuitionFees :: HS.Statement (Id "qualification", V.Vector PatchTuitionFees_Fees) ()
patchTuitionFees =
  lmap mkTpl $
  [resultlessStatement|
    insert into edgenode.provider_branch_qualification_tuition_fees
    (provider_branch_qualification_fk, position, amount, currency, period, countries)
    select $1 :: int8, pos,  am, curr, per, cs
    from unnest($2 ::  int4[], $3 :: float8[], $4 :: text[], $5 :: text[], $6 :: jsonb[])
    as x(pos, am, curr, per, cs)
    on conflict (currency, period, countries, provider_branch_qualification_fk)
    do update set amount = excluded.amount, position = excluded.position|]
  where
    mkTpl x = consT (x^._1.coerced) $ V.unzip5 $ V.mapMaybe mkTplFees $ V.zip (x^._2) (V.fromList [0 .. fromIntegral (V.length (x^._2)) - 1])
    mkTplFees (x, pos) = fmap (consT pos . mkTplFeesInfo) fees_info_m
      where (_, fees_info_m) = mkEncoderPatchTuitionFees_Fees x
    mkTplFeesInfo x = mkEncoderFeesInfo x & _2 %~ (^.currency) & _3 %~ (^.period) & _4 %~ toJSON . V.map (^.country)

deleteFees :: HS.Statement (Id "qualification", V.Vector Word64) ()
deleteFees =
  lmap (bimap coerce (V.map fromIntegral)) $
  [resultlessStatement|
    delete from edgenode.provider_branch_qualification_tuition_fees
    where id = any($2 :: int8[]) and provider_branch_qualification_fk = $1 :: int8|]

deleteClusters :: HS.Statement (Id "qualification") ()
deleteClusters = undefined

deleteDeps :: HS.Statement (Id "qualification") ()
deleteDeps = undefined

getDepsQualifiationValues :: HS.Statement (OnlyField "id" (Id "qualification")) (Maybe [(Int64, QualificationDegree, T.Text)])
getDepsQualifiationValues =
  lmap (coerce @_ @Int64) $
  statement $
  fmap wrapToMaybe (premap (& _2 %~ (toQualificationDegree . toS)) list)
  where
    wrapToMaybe [] = Nothing
    wrapToMaybe xs = Just xs
    statement =
      [foldStatement|
        with recursive dependencies as (
          select pbqd.provider_branch_qualification_fk as ident, pbqd.required_degree as val
          from edgenode.provider_branch_qualification_dependency as pbqd
          where pbqd.dependency_fk = $1 :: int8
          union all
          select pbqd.provider_branch_qualification_fk as ident, pbqd.required_degree as val
          from edgenode.provider_branch_qualification_dependency as pbqd
          join dependencies as deps
          on pbqd.dependency_fk = deps.ident)
        select
          distinct (deps.ident) :: int8,
          pbq.type :: text,
          val :: text
        from dependencies as deps
        inner join edgenode.provider_branch_qualification as pbq
        on deps.ident = pbq.id|]