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
import Test.Text.Parsing.Traffic    as Traffic
import Test.Text.Parsing.Flow       as Flow
import Test.Text.Parsing.Risk       as Risk
import Test.Text.Parsing.Statistics as Statistics
import Test.Text.Parsing.Linux      as Linux
import Test.Text.Parsing.Windows    as Windows
import Test.Text.Parsing.Forward    as Forward

data IssueCategory = DependenciesFFI | ParsingValidation

data Status = Assigned | Complete | Pending

main :: Effect Unit
main = void $ launchAff $ do
  {-- todo: data type for tree of issues and resolution suites --}
  _ <- render (h1 "--------------------\n- Issue Management -\n--------------------")
  {-- RELEASE-01/TIER-02/ISSUE-01 --}
  _ <- render (issue Pending DependenciesFFI)
  _ <- render (suite Assigned "FFI.Date")
  _ <- render (suite Assigned "FFI.HTTPS")
  _ <- render (suite Assigned "FFI.JSON")
  _ <- render (suite Assigned "FFI.Process")
  _ <- render (suite Assigned "FFI.Readline")
  _ <- render (suite Assigned "FFI.RSA")
  _ <- render (suite Assigned "FFI.SQLite3")
  _ <- render (suite Complete "FFI.UUID")                  *> UUID.suite
  {-- RELEASE-01/TIER-02/ISSUE-02 --}
  _ <- render (issue Pending ParsingValidation)
  _ <- render (suite Assigned  "Text.Parsing.Common")      *> Common.suite
  _ <- render (suite Pending   "Text.Parsing.Risk")        *> Risk.suite
  _ <- render (suite Complete  "Text.Parsing.Statistics")  *> Statistics.suite
  _ <- render (suite Assigned  "Text.Parsing.TCP.Flag")
  {-- RELEASE-01/TIER-02/ISSUE-03 --}
  _ <- render (issue Pending ParsingValidation)
  _ <- render (suite Complete  "Text.Parsing.Alert")       *> Alert.suite
  _ <- render (suite Complete  "Text.Parsing.Audit")       *> Audit.suite
  _ <- render (suite Complete  "Text.Parsing.Linux")       *> Linux.suite
  _ <- render (suite Complete  "Text.Parsing.Traffic")     *> Traffic.suite
  _ <- render (suite Complete  "Text.Parsing.Windows")     *> Windows.suite
  _ <- render (suite Complete  "Text.Parsing.Forward")     *> Forward.suite 
  _ <- render (issue Pending ParsingValidation)
  _ <- render (suite Assigned  "Text.Parsing.Report") 
  _ <- render (suite Assigned  "Text.Parsing.Route")
  _ <- render (issue Pending ParsingValidation)
  _ <- render (suite Complete  "Text.Parsing.Flow")        *> Flow.suite
  _ <- render (suite Assigned  "Text.Parsing.NetFlowv9")   *> NetFlowv9.suite
  _ <- render (issue Pending ParsingValidation)
  _ <- render (suite Assigned  "Text.Parsing.Linux.Audit")
  _ <- render (issue Pending ParsingValidation)
  _ <- render (suite Assigned  "Text.Parsing.Windows.Security")
  _ <- render (h1 "-------------------\n- Risk Management -\n-------------------")
  _ <- render (h2 "Risk/Audit")
  _ <- render (suite Assigned "Control.Tier3")
  _ <- render (suite Assigned "Control.Tier2")
  _ <- render (suite Assigned "Control.Tier1")
  _ <- render (h2 "Risk/Access-Control")
  _ <- render (suite Assigned "Control.Tier3")
  _ <- render (suite Assigned "Control.Tier2")
  _ <- render (suite Assigned "Control.Tier1")
  _ <- render (h2 "Risk/Availability")
  _ <- render (suite Complete "Control.Tier3")             *> Tier3.availability
  _ <- render (suite Assigned "Control.Tier2")
  _ <- render (suite Assigned "Control.Tier1")
  _ <- render (h2 "Risk/Exposure")
  _ <- render (suite Assigned   "Control.Tier3")
  _ <- render (suite Assigned  "Control.Tier2")
  _ <- render (suite Assigned "Control.Tier1")
  _ <- render (h2 "Risk/Injection")
  _ <- render (suite Assigned "Control.Tier3")
  _ <- render (suite Assigned "Control.Tier2")
  _ <- render (suite Assigned "Control.Tier1")
  _ <- render (h2 "Risk/Scalability")
  _ <- render (suite Assigned "Control.Tier3")
  _ <- render (suite Assigned "Control.Tier2")
  _ <- render (suite Assigned "Control.Tier1")
  pure unit
  where
    suite         = \x y -> div [status "SUITE" $ x, label y $ x]
    issue         = \x y -> div [status "ISSUE" $ x, issueCategory y]
    status        = \x y -> case y of 
      Assigned -> dim  $ fgWhite x
      Pending  -> bold $ fgYellow x
      Complete -> bold $ fgGreen x
    label         = \x y -> case y of
      Assigned -> dim  $ fgWhite x
      Pending  -> bold $ fgCyan x  
      Complete -> bold $ fgCyan x
    issueCategory = \x -> case x of
      DependenciesFFI   -> bold $ fgRed "Dependencies/FFI"
      ParsingValidation -> bold $ fgRed "Parsing/Validation"
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
