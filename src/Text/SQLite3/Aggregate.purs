module Text.SQLite3.Aggregate
  ( maximum
  , minimum
  , average
  , sum
  , count
  , variance
  ) where

import Prelude

import Data.Foldable (intercalate)

maximum :: String -> String -> String -> String
maximum x y z = intercalate "" ["SELECT MAX(", x, ")", " ", "AS", " ", y, " ", "FROM", " ", "(", z, ")"]

minimum :: String -> String -> String -> String
minimum x y z = intercalate "" ["SELECT MIN(", x, ")", " ", "AS", " ", y, " ", "FROM", " ", "(", z, ")"]

average :: String -> String -> String -> String
average x y z = intercalate "" ["SELECT AVG(", x, ")", " ", "AS", " ", y, " ", "FROM", " ", "(", z, ")"]

sum :: String -> String -> String -> String
sum x y z = intercalate "" ["SELECT SUM(", x, ")", " ", "AS", " ", y, " ", "FROM", " ", "(", z, ")"]

count :: String -> String -> String -> String
count x y z = intercalate "" ["SELECT COUNT(", x, ")", " ", "AS", " ", y, " ", "FROM", " ", "(", z, ")"]

variance :: String -> String -> String -> Number -> String
variance x y z = \avg -> average (square (diff x (show avg))) y z
  where
    square w    = intercalate "" ["(", w, " ", "*", " ", w, ")" ]
    diff   u v  = intercalate "" ["(", u, " ", "-", " ", v, ")"]
