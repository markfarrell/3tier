module Data.Schema
  ( Schema(..)
  , schemas
  , create
  ) where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)

import Data.Foldable (intercalate)

data Schema = Alert | Audit | Traffic | Linux | Windows

data ColumnType = Text | Integer

instance showSchema :: Show Schema where
  show Alert   = "ALERT"
  show Audit   = "AUDIT"
  show Traffic = "TRAFFIC"
  show Linux   = "LINUX"
  show Windows = "WINDOWS"

derive instance eqSchema :: Eq Schema

schemas :: Array Schema
schemas = [ Alert, Audit, Traffic, Linux, Windows ]

create :: Schema -> String
create schema = query
  where
    query              = "CREATE TABLE IF NOT EXISTS " <> table <> " (" <> columns <> ")"
    table              = show schema
    columns            = (intercalate "," columns')
    columns'           = column <$> params
    column param       = intercalate " " $ [fst param, columnType $ snd param]
    columnType Text    = "TEXT NOT NULL"
    columnType Integer = "INTEGER NOT NULL"
    params =
        [ Tuple "EventCategory" Text
        , Tuple "EventID" Text
        , Tuple "EventType" Text
        , Tuple "SourceID" Text
        , Tuple "InstanceID" Text
        , Tuple "StartTime" Text
        , Tuple "Duration" Integer
        , Tuple "EndTime" Text
        ]

