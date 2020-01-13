module SIEM.Logging.Windows
  ( Entry(..)
  , parseEntry
  , insert
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
import Data.Tuple (Tuple(..))

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
  
insert :: String -> Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert filename (Entry entry) req = do
  timestamp <- lift $ liftEffect (Date.toISOString <$> Date.current)
  logID     <- lift $ liftEffect (createLogID)
  DB.insert filename table $ params timestamp logID
  where
    params timestamp logID =
      [ Tuple "Timestamp" timestamp
      , Tuple "RemoteAddress" remoteAddress
      , Tuple "RemotePort" remotePort'
      , Tuple "LogID" logID
      , Tuple "EntryID" (entryID logID)
      , Tuple "EventID" entry.eventID
      , Tuple "MachineName" entry.machineName
      , Tuple "EntryData" entry.entryData
      , Tuple "EntryIndex" entry.entryIndex
      , Tuple "Category" entry.category
      , Tuple "CategoryNumber" entry.categoryNumber
      , Tuple "EntryType" entry.entryType
      , Tuple "Message" entry.message
      , Tuple "Source" entry.source
      , Tuple "ReplacementStrings" entry.replacementStrings
      , Tuple "InstanceID" entry.instanceID
      , Tuple "TimeGenerated" entry.timeGenerated
      , Tuple "TimeWritten" entry.timeWritten
      , Tuple "UserName" entry.userName
      , Tuple "Site" entry.site
      , Tuple "Container" entry.container
      ]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    entryID logID = UUIDv3.namespaceUUID logID $ HTTP.messageURL req
    table = "Windows"
    createLogID' headers = runExcept $ do
      result <- headers ! "log-id" >>= Foreign.readString
      pure result
    createLogID = do
       result <- pure (createLogID' $ HTTP.messageHeaders req)
       case result of
         (Left _)      -> Exception.throw "Invalid request headers (Log-ID)."
         (Right logID) -> pure $ logID 

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
