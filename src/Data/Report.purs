module Data.Report
  ( Event(..)
  , ReportType(..)
  , events
  ) where

import Prelude

import Data.Foldable (intercalate)

import FFI.String as String

import Data.Audit as Audit
import Data.Event as Event

data ReportType = Source | Time

{-- todo: same format as Forward.event, e.g. SQLite3 LIKE clause on start time and end time? --}
data Event = Audit Audit.EventCategory Event.EventType Audit.EventID ReportType

instance showEventReport :: Show Event where
  show = uri

instance showReportType :: Show ReportType where
  show Source = "SOURCE"
  show Time   = "TIME"

reportTypes :: Array ReportType
reportTypes = [ Source, Time ]

events :: Array Event
events = do
  reportType    <- reportTypes
  eventCategory <- Audit.eventCategories
  eventType     <- Event.eventTypes
  eventID       <- Audit.eventIDs
  pure $ Audit eventCategory eventType eventID reportType 

uri :: Event -> String
uri (Audit eventCategory eventType eventID reportType) = intercalate "/" $
  [ "/report"
  , String.toLowerCase $ show eventCategory
  , String.toLowerCase $ show eventType
  , String.toLowerCase $ show eventID
  , String.toLowerCase $ show reportType
  ]
