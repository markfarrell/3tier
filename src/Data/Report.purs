module Data.Report
  ( URI(..)
  , ReportType(..)
  , uris
  ) where

import Prelude

import Data.Foldable (intercalate)

import FFI.String as String

import Data.Audit as Audit
import Data.Event as Event

data ReportType = Source | Time

data URI = Audit Audit.EventCategory Event.EventType Audit.EventID ReportType

instance showURIReport :: Show URI where
  show = uri

instance showReportType :: Show ReportType where
  show Source = "SOURCE"
  show Time   = "TIME"

reportTypes :: Array ReportType
reportTypes = [ Source, Time ]

uris :: Array URI
uris = do
  reportType    <- reportTypes
  eventCategory <- Audit.eventCategories
  eventType     <- Event.eventTypes
  eventID       <- Audit.eventIDs
  pure $ Audit eventCategory eventType eventID reportType 

uri :: URI -> String
uri (Audit eventCategory eventType eventID reportType) = intercalate "/" $
  [ "/report"
  , String.toLowerCase $ show eventCategory
  , String.toLowerCase $ show eventType
  , String.toLowerCase $ show eventID
  , String.toLowerCase $ show reportType
  ]
