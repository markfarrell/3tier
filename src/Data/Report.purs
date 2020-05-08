module Data.Report
  ( URI(..)
  , ReportType(..)
  , uris
  ) where

import Prelude

import Data.Foldable (intercalate)

import Data.Audit as Audit

data ReportType = Source | Time

data URI = Audit Audit.EventCategory Audit.EventType Audit.EventID ReportType

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
  eventType     <- Audit.eventTypes
  eventID       <- Audit.eventIDs
  pure $ Audit eventCategory eventType eventID reportType 

uri :: URI -> String
uri (Audit eventCategory eventType eventID reportType) = intercalate "/" $
  [ "/report"
  , show eventCategory
  , show eventType
  , show eventID
  , show reportType
  ]
