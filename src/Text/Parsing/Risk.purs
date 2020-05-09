module Text.Parsing.Risk
  ( injection
  ) where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice)

import Data.Risk (Risk)
import Data.Risk as Risk

import Text.Parsing.Common (substring)

injection :: Parser String Risk
injection = choice (injection' <$> ["'", "\"", "`"])

injection' :: String -> Parser String Risk
injection' quote = do
  _ <- substring quote
  _ <- keyword
  _ <- choice [comment, semicolon, substring quote]
  pure $ Risk.Injection
  where
    semicolon   = substring ";" 
    comment     = substring "--"

keyword :: Parser String String
keyword = choice (substring <$> keywords)

{-- Reference: https://www.sqlite.org/lang_keywords.html --}
keywords :: Array String
keywords = 
  [ "ABORT"
  , "ACTION"
  , "ADD"
  , "AFTER"
  , "ALL"
  , "ALTER"
  , "ALWAYS"
  , "ANALYZE"
  , "AND"
  , "AS"
  , "ASC"
  , "ATTACH"
  , "AUTOINCREMENT"
  , "BEFORE"
  , "BEGIN"
  , "BETWEEN"
  , "BY"
  , "CASCADE"
  , "CASE"
  , "CAST"
  , "CHECK"
  , "COLLATE"
  , "COLUMN"
  , "COMMIT"
  , "CONFLICT"
  , "CONSTRAINT"
  , "CREATE"
  , "CROSS"
  , "CURRENT"
  , "CURRENT_DATE"
  , "CURRENT_TIME"
  , "CURRENT_TIMESTAMP"
  , "DATABASE"
  , "DEFAULT"
  , "DEFERRABLE"
  , "DEFERRED"
  , "DELETE"
  , "DESC"
  , "DETACH"
  , "DISTINCT"
  , "DO"
  , "DROP"
  , "EACH"
  , "ELSE"
  , "END"
  , "ESCAPE"
  , "EXCEPT"
  , "EXCLUDE"
  , "EXCLUSIVE"
  , "EXISTS"
  , "EXPLAIN"
  , "FAIL"
  , "FILTER"
  , "FIRST"
  , "FOLLOWING"
  , "FOR"
  , "FOREIGN"
  , "FROM"
  , "FULL"
  , "GENERATED"
  , "GLOB"
  , "GROUP"
  , "GROUPS"
  , "HAVING"
  , "IF"
  , "IGNORE"
  , "IMMEDIATE"
  , "IN"
  , "INDEX"
  , "INDEXED"
  , "INITIALLY"
  , "INNER"
  , "INSERT"
  , "INSTEAD"
  , "INTERSECT"
  , "INTO"
  , "IS"
  , "ISNULL"
  , "JOIN"
  , "KEY"
  , "LAST"
  , "LEFT"
  , "LIKE"
  , "LIMIT"
  , "MATCH"
  , "NATURAL"
  , "NO"
  , "NOT"
  , "NOTHING"
  , "NOTNULL"
  , "NULL"
  , "NULLS"
  , "OF"
  , "OFFSET"
  , "ON"
  , "OR"
  , "ORDER"
  , "OTHERS"
  , "OUTER"
  , "OVER"
  , "PARTITION"
  , "PLAN"
  , "PRAGMA"
  , "PRECEDING"
  , "PRIMARY"
  , "QUERY"
  , "RAISE"
  , "RANGE"
  , "RECURSIVE"
  , "REFERENCES"
  , "REGEXP"
  , "REINDEX"
  , "RELEASE"
  , "RENAME"
  , "REPLACE"
  , "RESTRICT"
  , "RIGHT"
  , "ROLLBACK"
  , "ROW"
  , "ROWS"
  , "SAVEPOINT"
  , "SELECT"
  , "SET"
  , "TABLE"
  , "TEMP"
  , "TEMPORARY"
  , "THEN"
  , "TIES"
  , "TO"
  , "TRANSACTION"
  , "TRIGGER"
  , "UNBOUNDED"
  , "UNION"
  , "UNIQUE"
  , "UPDATE"
  , "USING"
  , "VACUUM"
  , "VALUES"
  , "VIEW"
  , "VIRTUAL"
  , "WHEN"
  , "WHERE"
  , "WINDOW"
  , "WITH"
  , "WITHOUT"
  ]