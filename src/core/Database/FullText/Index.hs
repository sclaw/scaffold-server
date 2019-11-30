{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

{- PostgreSQL database support full-text search,
 - in number of languages. Unfortunatelly, groundhog-postgresql
 - library does not expose these features in type-safe manner,
 - so here is code, required to create apporiate indexes.
 -}
module Database.FullText.Index 
       ( Language(..)
       , QueryString
       , pattern QueryString
       , providerDropIndexQuery
       , providerCreateIndexQuery
       , providerSearchQuery
       , qualificationSearchQuery
       , recreateIndex
       ) where

import EdgeNode.Lang
import EdgeNode.Provider

import TH.Proto (fromLanguage)
import Data.Char
import Data.Coerce
import Data.List
import Data.Tagged
import Data.String.Interpolate
import Language.Haskell.Printf
import Data.Typeable


data TableD
data FieldD
data IndexD
data QueryD

type TableName = Tagged TableD String
type FieldName = Tagged FieldD String
type IndexName = Tagged IndexD String
type QueryString = Tagged QueryD String

{-# COMPLETE TableName #-}
pattern TableName :: String -> TableName
pattern TableName a = Tagged a

{-# COMPLETE FieldName #-}
pattern FieldName :: String -> FieldName
pattern FieldName a = Tagged a

{-# COMPLETE QueryString #-}
pattern QueryString :: String -> QueryString
pattern QueryString a = Tagged a

{-# COMPLETE IndexName #-}
pattern IndexName :: String -> IndexName
pattern IndexName a = Tagged a

-- Derived 'Show' instance returns capitalized string, 
-- whilst PostgreSQL expects lowercased.
langStr :: Language -> String
langStr = fromLanguage

-- List of fields in `provider' table, that should be indexed for full-text search
providerFtFs :: [FieldName]
providerFtFs = [ "providerTitle", "providerCountry" ]

providerSelectFs :: [FieldName]
providerSelectFs = [ "id", "providerTitle", "providerCountry" ]

indexName :: Language -> TableName -> IndexName
indexName lang (TableName name) = IndexName [i|#{name}_ftindex_#{langStr lang}|]

-- Generate string like following:
-- coalesce(title,'') || ' ' || coalesce(body,''))
-- from list of fields names. For more information, see
-- https://www.postgresql.org/docs/9.5/static/textsearch-tables.html#TEXTSEARCH-TABLES-INDEX
indexBody :: [FieldName] -> String
indexBody fields = foldl1' fn (map coalesce $ coerce fields) 
  where coalesce = [s| coalesce("%s",'') |]
        fn f1 f2 = f1 ++ " || ' ' || " ++ f2

dropIndexQuery :: Language -> TableName -> QueryString
dropIndexQuery lang name = QueryString $ "drop index if exists " ++ "\"edgeNode\"." ++ map toLower index
  where IndexName index = indexName lang name

createIndexQuery :: Language -> TableName -> [FieldName] -> QueryString
createIndexQuery lang name fields = 
  QueryString 
  [i|create index #{map toLower index} 
     on "edgeNode"."#{untag name}" 
     using gin (to_tsvector('#{langStr lang}', 
     #{indexBody fields}));|]
  where IndexName index = indexName lang name

-- It is important to quote field names, since Postgres implicitly
-- converts all non-quoted identifiers to lower case.  Since Groundhog
-- creates tables with columns, named in camelCase style, it is
-- important.
--
-- See more:
-- https://stackoverflow.com/questions/37910287/sql-hint-to-reference-a-column

-- Luckily, no escaping is needed.
quote :: FieldName -> String
quote (FieldName s) = "\"" ++ s ++ "\""

createSearchQuery 
  :: Maybe Language
  -> [FieldName] -- ^ list of full-text indexed fields.
  -> [FieldName] -- ^ list of fields to select
  -> TableName
  -> QueryString
createSearchQuery langm fields selects name =
  let select :: String
      select = intercalate "," (map quote selects)
      lang = maybe "simple" langStr langm
  in QueryString
     [i|select #{select} 
        from "edgeNode"."#{untag name}" 
        where to_tsvector
        ('#{lang}', 
         #{indexBody fields}) @@ 
         to_tsquery(?);
     |]

providerDropIndexQuery :: Language -> QueryString
providerDropIndexQuery lang = dropIndexQuery lang [i|#{show (typeOf (undefined :: Provider))}|]
     
providerCreateIndexQuery :: Language -> QueryString
providerCreateIndexQuery lang = createIndexQuery lang [i|#{show (typeOf (undefined :: Provider))}|] providerFtFs
     
providerSearchQuery :: Maybe Language -> QueryString
providerSearchQuery lang = createSearchQuery lang providerFtFs providerSelectFs [i|#{show (typeOf (undefined :: Provider))}|]

qualificationSearchQuery :: Maybe Language -> QueryString
qualificationSearchQuery _ = undefined

recreateIndex :: (Language -> QueryString) -> (Language -> QueryString) -> [(String, String)]
recreateIndex drop create = 
  flip map [LanguageDanish .. 
            LanguageTurkish] $ \lang ->
    let QueryString drop' = drop lang
        QueryString create' = create lang
    in (drop', create')