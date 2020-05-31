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

import Test.Text.Parsing.Common       as Common
import Test.Text.Parsing.Alphanumeric as Alphanumeric
import Test.Text.Parsing.Repeat       as Repeat
import Test.Text.Parsing.Alert        as Alert
import Test.Text.Parsing.Audit        as Audit
import Test.Text.Parsing.Traffic      as Traffic
import Test.Text.Parsing.Flow         as Flow
import Test.Text.Parsing.Risk         as Risk
import Test.Text.Parsing.Statistics   as Statistics
import Test.Text.Parsing.Linux        as Linux
import Test.Text.Parsing.Windows      as Windows
import Test.Text.Parsing.Forward      as Forward

import Test.Text.Parsing.Linux.Audit  as L

{-- todo: issue data type --}
data IssueCategory = DependenciesFFI | ParsingValidation | RisksAvailability

data Status = Tracked | Pending | Expired | Complete

data Priority = None | Low | Medium | High | Top

main :: Effect Unit
main = void $ launchAff $ do
  _ <- render (h1 "--------------------\n- Issue Management -\n--------------------")
  {-- DEPENDENCIES/FFI --}
  _ <- UUID.suite
  {-- PARSING/VALIDATION --}
  _ <- render (issue Pending Medium ParsingValidation 29)
  _ <- Common.suite
  _ <- Alphanumeric.suite
  _ <- Repeat.suite
  {-- PARSING/VALIDATION --}
  _ <- Risk.suite
  _ <- Statistics.suite
  {-- PARSING/VALIDATION --}
  _ <- render (issue Tracked Top ParsingValidation 18)
  _ <- L.suite
  {-- PARSING/VALIDATION --}
  _ <- render (issue Expired High ParsingValidation 5)
  _ <- Alert.suite
  _ <- Audit.suite
  _ <- Traffic.suite
  _ <- Linux.suite
  _ <- Windows.suite
  _ <- Forward.suite
  {-- PARSING/VALIDATION --}
  _ <- render (issue Tracked Medium ParsingValidation 16)
  {-- PARSING/VALIDATION --}
  _ <- render (issue Complete Medium ParsingValidation 17)
  _ <- Flow.suite
  {-- PARSING/VALIDATION --}
  _ <- NetFlowv9.suite
  {-- PARSING/VALIDATION --}
  _ <- render (issue Tracked High ParsingValidation 19)
  {-- RISKS/AVAILABILITY --}
  _ <- render (issue Pending Medium RisksAvailability 13)
  _ <- Tier3.availability
  pure unit
  where
    issue         = \w x y z -> div [status w, priority x, issueCategory y, issueID z]
    priority      = \x -> case x of
      Top    -> bold $ fgRed     "TOP   "
      High   -> bold $ fgYellow  "HIGH  "
      Medium -> bold $ fgMagenta "MEDIUM" 
      Low    -> bold $ fgGreen   "LOW   "
      None   -> bold $ fgWhite   "NONE  "
    status        = \x -> case x of 
      Tracked  -> bold $ fgYellow  "ASSIGNED "
      Pending  -> bold $ fgGreen   "ASSIGNED "
      Expired  -> bold $ fgMagenta "RESOLVED "
      Complete -> bold $ fgGreen   "RESOLVED "
    issueCategory = \x -> case x of
      DependenciesFFI   -> bold $ fgWhite "DEPENDENCIES/FFI  "
      ParsingValidation -> bold $ fgWhite "PARSING/VALIDATION"
      RisksAvailability -> bold $ fgWhite "RISK/AVAILABILITY "
    issueID       = \x -> dim $ fgWhite (intercalate "" ["(", "/markfarrell/3tier/issues/", show x, ")"])
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
