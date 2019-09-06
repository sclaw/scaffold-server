{-# LANGUAGE OverloadedStrings #-}

module Database.Render (rawSql, condition) where

import Database.Groundhog.Generic.Sql
import Control.Lens.Iso.Extended
import Control.Lens
import Database.Groundhog.Core
import Database.Groundhog.Postgresql

condition :: Cond Postgresql r -> [PersistValue] -> String
condition cond xs = fromUtf8 (maybe mempty mkString (renderCond (RenderConfig id) cond))^.from textbs.from stext
  where mkString r = getQuery r <> intercalateS "," (map (fromString . defaultShowPrim) (getValues r xs)) 
      

-- | Database.Render:rawSql
--
-- >>> rawSql "select * from table where id = ?" [PersistInt64 1]
-- "select * from table where id = ? (1)"
rawSql :: String -> [PersistValue] -> String
rawSql sql xs = sql <> " " <> (fromUtf8 $ getQuery $ "(" <> commasJoin (map (fromString . defaultShowPrim) xs) <> ")")^.from textbs.from stext