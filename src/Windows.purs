module Windows
  ( Entry(..)
  , parseEntry
  , insert
  , summary
  , createReader
  ) where

import Prelude

import Control.Coroutine (Producer)
import Control.Monad.Trans.Class (lift)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Data.Foldable(foldl)

import Data.Traversable(foldMap)
import Data.String.CodeUnits (singleton)
import Data.List(many)

import Foreign (Foreign)
import Foreign (F, readString) as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (string, satisfy)

import CSVParser as CSVParser
import Stream as Stream

import Date as Date
import DB as DB
import HTTP as HTTP
import Socket as Socket
import Strings as Strings

newtype Entry = Entry
  { eventID :: String
  , machineName :: String
  , entryData :: String
  , entryIndex :: String
  , category :: String
  , categoryNumber :: String
  , entryType :: String
  , message :: String
  , source :: String
  , replacementStrings :: String
  , instanceID :: String
  , timeGenerated :: String
  , timeWritten :: String
  , userName :: String
  , site :: String
  , container :: String
  } 

instance showEntry :: Show Entry where
  show (Entry entry) = "(Entry " <> show entry <> ")"

parseValue :: Parser String String
parseValue = foldMap singleton <$> many (satisfy $ not <<< eq ',')

parseEntry :: Parser String Entry
parseEntry = do
  eventID        <- parseValue
  _              <- string ","
  machineName    <- parseValue
  _              <- string ","
  entryData      <- parseValue
  _              <- string ","
  entryIndex     <- parseValue
  _              <- string ","
  category       <- parseValue 
  _              <- string ","
  categoryNumber <- parseValue
  _              <- string ","
  entryType      <- parseValue
  _              <- string ","
  message        <- parseValue
  _              <- string ","
  source         <- parseValue
  _              <- string ","
  replacementStr <- parseValue
  _              <- string ","
  instanceID     <- parseValue
  _              <- string ","
  timeGenerated  <- parseValue
  _              <- string "," 
  timeWritten    <- parseValue
  _              <- string ","
  userName       <- parseValue
  _              <- string ","
  site           <- parseValue
  _              <- string ","
  container      <- parseValue
  pure $ Entry
    { eventID : eventID
    , machineName : machineName
    , entryData : entryData
    , entryIndex : entryIndex
    , category : category
    , categoryNumber : categoryNumber
    , entryType : entryType
    , message : message
    , source : source
    , replacementStrings : replacementStr
    , instanceID : instanceID
    , timeGenerated : timeGenerated
    , timeWritten : timeWritten
    , userName : userName
    , site : site
    , container : container
    }
  
entryQuery :: Entry -> HTTP.IncomingMessage -> Effect String
entryQuery (Entry entry) req = do
  timestamp <- Date.toISOString <$> Date.current
  pure $ query timestamp
  where
    query timestamp = "INSERT INTO Windows (Timestamp, RemoteAddress, RemotePort, URL, EventID) VALUES ('" <> values timestamp <> "')"
    values timestamp = foldl (\x y -> x <> "','" <> y) timestamp $ [remoteAddress, remotePort', url', eventID]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    url' = Strings.encodeBase64 $ HTTP.messageURL req
    eventID = entry.eventID

insert :: Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert entry = \req -> do
  query <- lift $ liftEffect $ entryQuery entry req
  DB.insert filename query
  where filename = "logs.db"

summary :: DB.Request (Array (Array String))
summary = DB.select filename query readResult
  where
    readResult row = do
       taskCategory <- row ! "TaskCategory" >>= Foreign.readString
       entries      <- row ! "Entries"      >>= Foreign.readString
       pure $ [taskCategory, entries]
    query = "SELECT x.TaskCategory AS 'TaskCategory', CAST(SUM(y.Entries) AS TEXT) AS 'Entries' FROM TaskCategories as x INNER JOIN"
      <> " (SELECT EventID, COUNT (DISTINCT URL) as 'Entries' FROM Windows GROUP BY EventID) AS y"
      <> " ON y.EventID=x.EventID GROUP BY x.TaskCategory ORDER BY x.TaskCategory,y.Entries DESC;"
    filename = "logs.db"

readEntry :: Foreign -> Foreign.F Entry
readEntry row = do
  eventID            <- row ! "eventID"            >>= Foreign.readString
  machineName        <- row ! "machineName"        >>= Foreign.readString
  entryData          <- row ! "entryData"          >>= Foreign.readString
  entryIndex         <- row ! "entryIndex"         >>= Foreign.readString
  category           <- row ! "category"           >>= Foreign.readString
  categoryNumber     <- row ! "categoryNumber"     >>= Foreign.readString
  entryType          <- row ! "entryType"          >>= Foreign.readString
  message            <- row ! "message"            >>= Foreign.readString
  source             <- row ! "source"             >>= Foreign.readString
  replacementStrings <- row ! "replacementStrings" >>= Foreign.readString
  instanceID         <- row ! "instanceID"         >>= Foreign.readString
  timeGenerated      <- row ! "timeGenerated"      >>= Foreign.readString
  timeWritten        <- row ! "timeWritten"        >>= Foreign.readString
  userName           <- row ! "userName"           >>= Foreign.readString
  site               <- row ! "site"               >>= Foreign.readString
  container          <- row ! "container"          >>= Foreign.readString
  pure $ Entry
    { eventID : eventID
    , machineName : machineName
    , entryData : entryData
    , entryIndex : entryIndex
    , category : category
    , categoryNumber : categoryNumber
    , entryType : entryType
    , message : message
    , source : source
    , replacementStrings : replacementStrings
    , instanceID : instanceID
    , timeGenerated : timeGenerated
    , timeWritten : timeWritten
    , userName : userName
    , site : site
    , container : container
    }

createReader :: Stream.Readable -> Effect (Producer Entry Aff Unit)
createReader readable = createReader' readable $
  { separator : ","
  , headers :
      [ "eventID"
      , "machineName"
      , "entryData"
      , "entryIndex"
      , "category"
      , "categoryNumber"
      , "entryType"
      , "message"
      , "source"
      , "replacementStrings"
      , "instanceID"
      , "timeGenerated"
      , "timeWritten"
      , "userName"
      , "site"
      , "container"
      ]
  }
  where createReader' = CSVParser.createReader readEntry

writeEntry :: Entry -> String
writeEntry (Entry entry) = foldl (\x y -> x <> "," <> y) (show entry.eventID) $
  [ show entry.machineName
  , show entry.entryData
  , show entry.entryIndex
  , show entry.category      
  , show entry.categoryNumber
  , show entry.entryType
  , show entry.message
  , show entry.source
  , show entry.replacementStrings
  , show entry.instanceID
  , show entry.timeGenerated
  , show entry.timeWritten
  , show entry.userName
  , show entry.site
  , show entry.container
  ]

