module Test.Main
  ( main
  ) where

import Prelude

import Data.Foldable (intercalate)

import Effect         (Effect)
import Effect.Aff     (launchAff)
import Effect.Class   (liftEffect)
import Effect.Console (log)

import Test.Control.Tier3        as Tier3
import Test.Text.Parsing.Common  as Common
import Test.Text.Parsing.Flow    as Flow
import Test.Text.Parsing.Risk    as Risk
import Test.Text.Parsing.Windows as Windows

main :: Effect Unit
main = void $ launchAff $ do
  _ <- render (h1 "--------------------\n- Issue Management -\n--------------------")
  _ <- render (h2 "Dependencies/FFI")
  _ <- render (assigned  suite deliverable' "FFI.HTTPS")
  _ <- render (assigned  suite deliverable' "FFI.RSA")
  _ <- render (assigned  suite deliverable' "FFI.SQLite3")
  _ <- render (h2 "Design/Philosophy")
  _ <- render (assigned  suite deliverable' "Data.Event")               *> wip
  _ <- render (h2 "Parsing/Validation")
  _ <- render (assigned  suite deliverable' "Text.Parsing.Common")      *> Common.suite
  _ <- render (assigned  suite deliverable' "Text.Parsing.Alert")       *> wip
  _ <- render (assigned  suite deliverable' "Text.Parsing.Audit")       *> wip
  _ <- render (pending   suite deliverable' "Text.Parsing.Flow")        *> Flow.suite
  _ <- render (assigned  suite deliverable' "Text.Parsing.Linux")       *> wip
  _ <- render (assigned  suite deliverable' "Text.Parsing.Report")      *> wip
  _ <- render (assigned  suite deliverable' "Text.Parsing.Risk")        *> Risk.suite
  _ <- render (assigned  suite deliverable' "Text.Parsing.Windows")     *> Windows.suite
  _ <- render (assigned  suite deliverable' "Text.Parsing.Route")       *> wip
  _ <- render (h1 "-------------------\n- Risk Management -\n-------------------")
  _ <- render (h2 "Risk/Audit")
  _ <- render (assigned  suite deliverable   "Control.Tier3")           *> wip
  _ <- render (assigned  suite deliverable'  "Control.Tier2")           *> wip
  _ <- render (assigned  suite deliverable'' "Control.Tier1")           *> wip
  _ <- render (h2 "Risk/Access-Control")
  _ <- render (assigned  suite deliverable   "Control.Tier3")           *> wip
  _ <- render (assigned  suite deliverable'  "Control.Tier2")           *> wip
  _ <- render (assigned  suite deliverable'' "Control.Tier1")           *> wip
  _ <- render (h2 "Risk/Availability")
  _ <- render (complete  suite deliverable   "Control.Tier3")           *> Tier3.suite
  _ <- render (assigned  suite deliverable'  "Control.Tier2")           *> wip
  _ <- render (assigned  suite deliverable'' "Control.Tier1")           *> wip
  _ <- render (h2 "Risk/Exposure")
  _ <- render (assigned  suite deliverable   "Control.Tier3")           *> wip
  _ <- render (assigned  suite deliverable'  "Control.Tier2")           *> wip
  _ <- render (assigned  suite deliverable'' "Control.Tier1")           *> wip
  _ <- render (h2 "Risk/Injection")
  _ <- render (assigned  suite deliverable   "Control.Tier3")           *> wip
  _ <- render (assigned  suite deliverable'  "Control.Tier2")           *> wip
  _ <- render (assigned  suite deliverable'' "Control.Tier1")           *> wip
  _ <- render (h2 "Risk/Scalability")
  _ <- render (assigned  suite deliverable   "Control.Tier3")           *> wip
  _ <- render (assigned  suite deliverable'  "Control.Tier2")           *> wip
  _ <- render (assigned  suite deliverable'' "Control.Tier1")           *> wip
  pure unit
  where 
    deliverable   = "RELEASE-01/TIER-03"
    deliverable'  = "RELEASE-01/TIER-02"
    deliverable'' = "RELEASE-01/TIER-01"
    assigned      = \x _ z -> div [dim $ fgWhite x, dim $ fgWhite z]
    pending       = \x _ z -> div [bold $ fgYellow x, bold $ fgCyan z]
    complete      = \x _ z -> div [bold $ fgGreen x, bold $ fgCyan z]
    suite         = "SUITE"
    wip           = pure unit 
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
