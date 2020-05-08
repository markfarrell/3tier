module Test.Main
  ( main
  ) where

import Prelude

import Data.Foldable (intercalate)

import Effect         (Effect)
import Effect.Aff     (launchAff)
import Effect.Class   (liftEffect)
import Effect.Console (log)

import Test.FFI.UUID as UUID

import Test.Control.Tier3           as Tier3
import Test.Data.NetFlowv9          as NetFlowv9

import Test.Text.Parsing.Common     as Common
import Test.Text.Parsing.Alert      as Alert
import Test.Text.Parsing.Audit      as Audit
import Test.Text.Parsing.Flow       as Flow
import Test.Text.Parsing.Risk       as Risk
import Test.Text.Parsing.Statistics as Statistics
import Test.Text.Parsing.Windows    as Windows

main :: Effect Unit
main = void $ launchAff $ do
  _ <- render (h1 "--------------------\n- Issue Management -\n--------------------")
  {-- RELEASE-01/TIER-02/ISSUE-01 --}
  _ <- render (h2 "Dependencies/FFI")
  _ <- render (assigned suite "FFI.Date")
  _ <- render (assigned suite "FFI.HTTPS")
  _ <- render (assigned suite "FFI.JSON")
  _ <- render (assigned suite "FFI.Process")
  _ <- render (assigned suite "FFI.Readline")
  _ <- render (assigned suite "FFI.RSA")
  _ <- render (assigned suite "FFI.SQLite3")
  _ <- render (complete suite "FFI.UUID")                  *> UUID.suite
  _ <- render (h2 "Design/Control")
  _ <- render (assigned suite "Control.DSL")
  _ <- render (h2 "Design/Data")
  _ <- render (assigned suite "Data.Event")
  _ <- render (pending suite "Data.NetFlowv9")             *> NetFlowv9.suite
  {-- RELEASE-01/TIER-02/ISSUE-02 --}
  _ <- render (h2 "Parsing/Validation")
  _ <- render (assigned suite  "Text.Parsing.Common")      *> Common.suite
  _ <- render (complete suite  "Text.Parsing.Alert")       *> Alert.suite
  _ <- render (complete suite  "Text.Parsing.Audit")       *> Audit.suite
  _ <- render (complete  suite "Text.Parsing.Flow")        *> Flow.suite
  _ <- render (assigned suite  "Text.Parsing.Linux")
  _ <- render (complete suite  "Text.Parsing.Statistics")  *> Statistics.suite
  _ <- render (assigned suite "Text.Parsing.Traffic")
  _ <- render (pending suite   "Text.Parsing.Risk")        *> Risk.suite
  _ <- render (pending suite   "Text.Parsing.Windows")     *> Windows.suite
  _ <- render (assigned suite  "Text.Parsing.Forward") 
  _ <- render (assigned suite  "Text.Parsing.Report") 
  _ <- render (assigned suite  "Text.Parsing.Route")
  _ <- render (h1 "-------------------\n- Risk Management -\n-------------------")
  _ <- render (h2 "Risk/Audit")
  _ <- render (assigned suite "Control.Tier3")
  {-- RELEASE-01/TIER-02/ISSUE-03 --}
  _ <- render (assigned suite "Control.Tier2")
  _ <- render (assigned suite "Control.Tier1")
  _ <- render (h2 "Risk/Access-Control")
  _ <- render (assigned suite "Control.Tier3")
  _ <- render (assigned suite "Control.Tier2")
  _ <- render (assigned suite "Control.Tier1")
  _ <- render (h2 "Risk/Availability")
  _ <- render (complete suite "Control.Tier3")             *> Tier3.availability
  _ <- render (assigned suite "Control.Tier2")
  _ <- render (assigned suite "Control.Tier1")
  _ <- render (h2 "Risk/Exposure")
  _ <- render (assigned suite   "Control.Tier3")
  _ <- render (assigned suite  "Control.Tier2")
  _ <- render (assigned suite "Control.Tier1")
  _ <- render (h2 "Risk/Injection")
  _ <- render (assigned suite "Control.Tier3")
  _ <- render (assigned suite "Control.Tier2")
  _ <- render (assigned suite "Control.Tier1")
  _ <- render (h2 "Risk/Scalability")
  _ <- render (assigned suite "Control.Tier3")
  _ <- render (assigned suite "Control.Tier2")
  _ <- render (assigned suite "Control.Tier1")
  pure unit
  where
    assigned      = \x y -> div [dim $ fgWhite x, dim $ fgWhite y]
    pending       = \x y -> div [bold $ fgYellow x, bold $ fgCyan y]
    complete      = \x y -> div [bold $ fgGreen x, bold $ fgCyan y]
    suite         = "SUITE"
    render        = liftEffect <<< log
    div           = \x -> intercalate " " $ x
    h1            = bold <<< fgYellow
    h2            = bold <<< fgRed
    square        = \x ->  (fgGreen "[") <> x <>  (fgGreen "]")
    round         = \x ->  (fgGreen "(") <> x <>  (fgGreen ")")
    dim           = \x -> "\x1b[2m" <> x <> "\x1b[0m"
    bold          = \x -> "\x1b[1m" <> x <> "\x1b[0m"
    fgCyan        = \x -> "\x1b[36m" <> x <> "\x1b[0m"
    fgRed         = \x -> "\x1b[31m" <> x <> "\x1b[0m"
    fgGreen       = \x -> "\x1b[32m" <> x <> "\x1b[0m"
    fgYellow      = \x -> "\x1b[33m" <> x <> "\x1b[0m"
    fgWhite       = \x -> "\x1b[37m" <> x <> "\x1b[0m"
    fgMagenta     = \x -> "\x1b[35m" <> x <> "\x1b[0m"
    underline     = \x -> "\x1b[4m" <> x <> "\x1b[0m"
    {-- Reference: https://stackoverflow.com/questions/9781218/how-to-change-node-jss-console-font-color --}
