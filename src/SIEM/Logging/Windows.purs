module SIEM.Logging.Windows
  where

import Prelude

import Control.Coroutine (Consumer, pullFrom, await, runProcess)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Array (drop) as Array

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Effect.Aff (Aff, launchAff)

import Foreign (Foreign)

import CSVParser as CSVParser

import Process as Process

log :: String -> Aff Unit
log = liftEffect <<< Console.log

consumer :: Consumer Foreign Aff Unit
consumer = forever $ do
  value <- await
  lift $ pure unit

main :: Effect Unit
main = do
  case argv' of
    _ -> do
      reader <- CSVParser.createReader Process.stdin $
        { headers : 
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
      void $ launchAff $ runProcess (pullFrom consumer reader)
  where argv'  = Array.drop 2 Process.argv
