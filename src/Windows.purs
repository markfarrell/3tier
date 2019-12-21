module Windows
  ( Entry(..)
  , parseEntry
  , insert
  , summary
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)

import Effect (Effect)
import Effect.Class (liftEffect)

import Data.Traversable(foldMap)
import Data.String.CodeUnits (singleton)
import Data.List(many)

import Foreign (readString) as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (string, satisfy)

import Date as Date
import DB as DB
import HTTP as HTTP
import Socket as Socket
import UUIDv3 as UUIDv3

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
  timestamp <- Date.now
  pure $ query timestamp
  where
    query timestamp = "INSERT INTO Windows (UUID, Timestamp, RemoteAddress, RemotePort, EventID, MachineName, EntryData, EntryIndex, Category, CategoryNumber, EntryType, Message, Source, ReplacementStrings, InstanceID, TimeGenerated, TimeWritten, UserName, Site, Container)" <> " " <> "VALUES" <> " " <> "(" <> values timestamp <> ")"
    values timestamp = "'" <> uuid <> "','" <> show timestamp <> "','" <> remoteAddress <> "','" <> show remotePort <> "','"  <> entry.eventID <> "','" <> entry.machineName <> "','" <> entry.entryData <> "','" <> entry.category <> "','" <> entry.categoryNumber <> "','" <> entry.entryType <> "','" <> entry.message <> "','" <> entry.source <> "','" <> entry.replacementStrings <> "','" <> entry.instanceID <> "','" <> entry.timeGenerated <> "','" <> entry.timeWritten <> "','" <> entry.userName <> "','"
          <> entry.userName <> "','" <>  entry.site <> "','" <> entry.container <> "'"
    uuid = UUIDv3.url $ HTTP.messageURL req
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req

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
    query = "SELECT x.TaskCategory AS 'TaskCategory',SUM(y.Entries) AS 'Entries' FROM TaskCategories as x INNER JOIN"
      <> " (SELECT EventID, COUNT (DISTINCT UUID) as 'Entries' FROM Windows GROUP BY EventID) AS y"
      <> " ON y.EventID=x.EventID GROUP BY x.TaskCategory ORDER BY x.TaskCategory,y.Entries DESC;"
    filename = "logs.db"
