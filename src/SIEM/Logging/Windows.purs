module SIEM.Logging.Windows
  ( Entry(..)
  , parseEntry
  , insert
  , summary
  , createReader
  , writeEntry
  ) where

import Prelude

import Control.Coroutine (Producer)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (runExcept)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Effect.Exception (Error)
import Effect.Exception (error, throw) as Exception

import Data.Either (Either(..))

import Data.Identity (Identity)
import Data.Newtype (unwrap)

import Data.Foldable(foldl)

import Data.Traversable(foldMap)
import Data.String.CodeUnits (singleton)
import Data.List(many)

import Foreign (Foreign)
import Foreign (F, readString) as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (char, string, satisfy)
import Text.Parsing.Parser.Combinators (between) as Combinators

import CSVParser as CSVParser
import Stream as Stream

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

instance eqEntry :: Eq Entry where
  eq (Entry entry) (Entry entry') = eq entry entry'

parseValue' :: Parser String String
parseValue' = foldMap singleton <$> many (satisfy $ not <<< eq '"')

parseValue :: Parser String String
parseValue = Combinators.between (char '"') (char '"') parseValue'

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
  
insertQuery :: Entry -> HTTP.IncomingMessage -> Effect String
insertQuery (Entry entry) req = do
  timestamp <- Date.toISOString <$> Date.current
  logID     <- createLogID
  pure $ query timestamp logID
  where
    query timestamp logID = "INSERT INTO Windows (" <> columns <> ") VALUES ('" <> values timestamp logID <> "')"
    columns = foldl (\x y -> x <> "," <> y) "Timestamp" $
      [ "RemoteAddress"
      , "RemotePort"
      , "LogID"
      , "EntryID"
      , "EventID"
      , "MachineName"
      , "EntryData"
      , "EntryIndex"
      , "Category"
      , "CategoryNumber"
      , "EntryType"
      , "Message"
      , "Source"
      , "ReplacementStrings"
      , "InstanceID"
      , "TimeGenerated"
      , "TimeWritten"
      , "UserName"
      , "Site"
      , "Container"
      ]
    values timestamp logID = foldl (\x y -> x <> "','" <> y) timestamp $
      [ remoteAddress
      , remotePort'
      , logID
      , entryID logID
      , entry.eventID
      , entry.machineName
      , entry.entryData
      , entry.entryIndex
      , entry.category
      , entry.categoryNumber
      , entry.entryType
      , entry.message
      , entry.source
      , entry.replacementStrings
      , entry.instanceID
      , entry.timeGenerated
      , entry.timeWritten
      , entry.userName
      , entry.site
      , entry.container
      ]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    entryID logID = UUIDv3.namespaceUUID logID $ HTTP.messageURL req
    createLogID' headers = runExcept $ do
      result <- headers ! "log-id" >>= Foreign.readString
      pure result
    createLogID = do
       result <- pure (createLogID' $ HTTP.messageHeaders req)
       case result of
         (Left _)      -> Exception.throw "Invalid request headers (Log-ID)."
         (Right logID) -> pure $ logID 

insert :: String -> Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert filename entry req = do
  query <- lift $ liftEffect $ insertQuery entry req
  DB.insert filename query

summary :: DB.Request (Array (Array String))
summary = DB.select filename query readResult
  where
    readResult row = do
       taskCategory <- row ! "TaskCategory" >>= Foreign.readString
       entries      <- row ! "Entries"      >>= Foreign.readString
       pure $ [taskCategory, entries]
    query = "SELECT y.RemoteAddress as 'RemoteAddress', x.TaskCategory AS 'TaskCategory', CAST(SUM(y.Entries) AS TEXT) AS 'Entries' FROM TaskCategories as x INNER JOIN"
      <> " (SELECT EventID, COUNT (DISTINCT URL) as 'Entries' FROM Windows GROUP BY EventID) AS y"
      <> " ON y.EventID=x.EventID GROUP BY y.RemoteAddress, x.TaskCategory ORDER BY y.Entries DESC;"
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

writeEntry'' :: Entry -> String
writeEntry'' (Entry entry) = foldl (\x y -> x <> "," <> y) (quote entry.eventID) $
  [ quote entry.machineName
  , quote entry.entryData
  , quote entry.entryIndex
  , quote entry.category      
  , quote entry.categoryNumber
  , quote entry.entryType
  , quote entry.message
  , quote entry.source
  , quote entry.replacementStrings
  , quote entry.instanceID
  , quote entry.timeGenerated
  , quote entry.timeWritten
  , quote entry.userName
  , quote entry.site
  , quote entry.container
  ]
  where quote value = "\"" <> value <> "\""

writeEntry' :: Entry -> Identity (Either Error String)
writeEntry' entry = do
  expect     <- pure $ writeEntry'' entry
  result'    <- pure $ flip runParser parseEntry $ (writeEntry'' entry)
  case result' of
    (Left error) -> do
      let result'' = { error : error, expect : expect, entry : entry }
      pure $ Left (Exception.error $ show result'') 
    (Right entry')    -> do
      check <- pure $ writeEntry'' entry'
      let result'' = { check  : check, expect : expect, entry : entry }
      case check == expect of
        true -> do
          pure $ Right expect
        false -> do
          pure $ Left (Exception.error $ show result'')

writeEntry :: Entry -> Either Error String
writeEntry entry = unwrap $ writeEntry' entry
