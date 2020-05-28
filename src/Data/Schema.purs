module Data.Schema
  ( Schema(..)
  , schemas
  , create
  , insert
  ) where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)

import Data.Foldable (intercalate)

import Data.Event (Event(..), class EventCategory, class EventID)

import Data.EventType (EventType)

import Data.Forward as Forward

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
        , Tuple "SessionID" Text
        , Tuple "DestinationID" Text
        , Tuple "LogID" Text
        , Tuple "SchemaID" Text
        , Tuple "FeatureID" Text
        , Tuple "InstanceID" Text
        , Tuple "StartTime" Text
        , Tuple "Duration" Integer
        , Tuple "EndTime" Text
        ]

insert' :: forall a b. EventCategory a => EventID b => Schema -> Event a EventType b -> String
insert' schema (Event event) = query
  where
    query    = "INSERT INTO " <> table <> " (" <> columns <> ") VALUES (" <> values <> ")"
    table    = show schema
    columns  = "'" <> (intercalate "','" columns') <> "'"
    values   = "'" <> (intercalate "','" values') <> "'"
    columns' = fst <$> params
    values'  = snd <$> params
    params   =
      [ Tuple "EventCategory" (show event.eventCategory)
      , Tuple "EventType" (show event.eventCategory)
      , Tuple "EventID" (show event.eventID)
      , Tuple "SourceID" (show event.sourceID)
      , Tuple "SessionID" (show event.sessionID)
      , Tuple "DestinationID" (show event.destinationID)
      , Tuple "LogID" (show event.logID)
      , Tuple "SchemaID" (show event.schemaID)
      , Tuple "FeatureID" (show event.featureID)
      , Tuple "InstanceID" (show event.instanceID)
      , Tuple "StartTime" (show $ event.startTime)
      , Tuple "Duration" (show $ event.duration)
      , Tuple "EndTime" (show $ event.endTime)
      ]

insert :: Forward.Event -> String
insert (Forward.Alert event)   = insert' Alert event
insert (Forward.Audit event)   = insert' Audit event
insert (Forward.Traffic event) = insert' Traffic event 
insert (Forward.Linux event)   = insert' Linux event 
insert (Forward.Windows event) = insert' Windows event 

