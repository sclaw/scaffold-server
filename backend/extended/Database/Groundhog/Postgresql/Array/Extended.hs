
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Database.Groundhog.Postgresql.Array.Extended
       ( module Database.Groundhog.Postgresql.Array
       , arrayReplace
       , arrayPosition
       , arrayPositions
       )
        where

import Database.Groundhog.Postgresql.Array
import Database.Groundhog.Core
import Database.Groundhog.Expression (ExpressionOf, toExpr)
import Database.Groundhog.Generic.Sql (mkExpr, function)
import Database.Groundhog.Postgresql (Postgresql)


arrayReplace :: ( ExpressionOf Postgresql r a (Array elem)
                , ExpressionOf Postgresql r b elem
                , ExpressionOf Postgresql r c elem
                ) =>
                a -> b -> c -> Expr Postgresql r (Array elem)
arrayReplace a b c = mkExpr $ function "array_replace" [toExpr a, toExpr b, toExpr c]

arrayPosition :: ( ExpressionOf Postgresql r a (Array elem)
                 , ExpressionOf Postgresql r b elem
                 ) =>
                 a -> b -> Expr Postgresql r Int
arrayPosition a b = mkExpr $ function "array_position" [toExpr a, toExpr b]

arrayPositions :: ( ExpressionOf Postgresql r a (Array elem)
                  , ExpressionOf Postgresql r b elem
                  ) =>
                  a -> b -> Expr Postgresql r [Int]
arrayPositions a b = mkExpr $ function "array_positions" [toExpr a, toExpr b]