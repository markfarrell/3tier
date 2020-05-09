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
import Test.Text.Parsing.Linux      as Linux
import Test.Text.Parsing.Windows    as Windows

main :: Effect Unit
main = void $ launchAff $ do
  _ <- render (h1 "--------------------\n- Issue Management -\n--------------------")
  {-- RELEASE-01/TIER-02/ISSUE-01 --}
  _ <- render (pendingIssue  "Dependencies/FFI")
  _ <- render (assignedSuite "FFI.Date")
  _ <- render (assignedSuite "FFI.HTTPS")
  _ <- render (assignedSuite "FFI.JSON")
  _ <- render (assignedSuite "FFI.Process")
  _ <- render (assignedSuite "FFI.Readline")
  _ <- render (assignedSuite "FFI.RSA")
  _ <- render (assignedSuite "FFI.SQLite3")
  _ <- render (completeSuite "FFI.UUID")                  *> UUID.suite
  {-- RELEASE-01/TIER-02/ISSUE-02 --}
  _ <- render (pendingIssue   "Parsing/Validation")
  _ <- render (assignedSuite  "Text.Parsing.Common")      *> Common.suite
  _ <- render (completeSuite  "Text.Parsing.Alert")       *> Alert.suite
  _ <- render (completeSuite  "Text.Parsing.Audit")       *> Audit.suite
  _ <- render (completeSuite  "Text.Parsing.Flow")        *> Flow.suite
  _ <- render (completeSuite  "Text.Parsing.Linux")       *> Linux.suite
  _ <- render (completeSuite  "Text.Parsing.Statistics")  *> Statistics.suite
  _ <- render (assignedSuite  "Text.Parsing.Traffic")
  _ <- render (pendingSuite   "Text.Parsing.Risk")        *> Risk.suite
  _ <- render (completeSuite  "Text.Parsing.Windows")     *> Windows.suite
  _ <- render (assignedSuite  "Text.Parsing.Forward") 
  _ <- render (assignedSuite  "Text.Parsing.Report") 
  _ <- render (assignedSuite  "Text.Parsing.Route")
  _ <- render (pendingIssue   "Logging/Traffic")
  _ <- render (pendingSuite   "Data.NetFlowv9")           *> NetFlowv9.suite
  _ <- render (h1 "-------------------\n- Risk Management -\n-------------------")
  _ <- render (h2 "Risk/Audit")
  _ <- render (assignedSuite "Control.Tier3")
    {-- RELEASE-01/TIER-02/ISSUE-03 --}
  _ <- render (assignedSuite "Control.Tier2")
  _ <- render (assignedSuite "Control.Tier1")
  _ <- render (h2 "Risk/Access-Control")
  _ <- render (assignedSuite "Control.Tier3")
  _ <- render (assignedSuite "Control.Tier2")
  _ <- render (assignedSuite "Control.Tier1")
  _ <- render (h2 "Risk/Availability")
  _ <- render (completeSuite "Control.Tier3")             *> Tier3.availability
  _ <- render (assignedSuite "Control.Tier2")
  _ <- render (assignedSuite "Control.Tier1")
  _ <- render (h2 "Risk/Exposure")
  _ <- render (assignedSuite   "Control.Tier3")
  _ <- render (assignedSuite  "Control.Tier2")
  _ <- render (assignedSuite "Control.Tier1")
  _ <- render (h2 "Risk/Injection")
  _ <- render (assignedSuite "Control.Tier3")
  _ <- render (assignedSuite "Control.Tier2")
  _ <- render (assignedSuite "Control.Tier1")
  _ <- render (h2 "Risk/Scalability")
  _ <- render (assignedSuite "Control.Tier3")
  _ <- render (assignedSuite "Control.Tier2")
  _ <- render (assignedSuite "Control.Tier1")
  pure unit
  where
    assignedSuite = \x -> div [dim $ fgWhite suite, dim $ fgWhite x]
    pendingSuite  = \x -> div [bold $ fgYellow suite, bold $ fgCyan x]
    completeSuite = \x -> div [bold $ fgGreen suite, bold $ fgCyan x]
    pendingIssue  = \x -> div [bold $ fgRed x]
    suite         = "SUITE"
    issue         = "ISSUE"
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
