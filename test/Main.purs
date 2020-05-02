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
  _ <- render (comment (div "TEST" "RELEASE-01/TIER-02" "Text.Parsing.Common"))      *> Common.suite
  _ <- render (comment (div "TEST" "RELEASE-01/TIER-02" "Text.Parsing.Alert"))       *> wip
  _ <- render (comment (div "TEST" "RELEASE-01/TIER-02" "Text.Parsing.Audit"))       *> wip
  _ <- render (div     "TEST" "RELEASE-01/TIER-02" "Text.Parsing.Flow")              *> Flow.suite
  _ <- render (comment (div "TEST" "RELEASE-01/TIER-02" "Text.Parsing.Linux"))       *> wip
  _ <- render (comment (div "TEST" "RELEASE-01/TIER-02" "Text.Parsing.Report"))      *> wip
  _ <- render (comment (div "TEST" "RELEASE-01/TIER-02" "Text.Parsing.Risk"))        *> Risk.suite
  _ <- render (comment (div     "TEST" "RELEASE-01/TIER-02" "Text.Parsing.Windows")) *> Windows.suite
  _ <- render (div     "TEST" "RELEASE-01/TIER-03" "Control.Tier3")                  *> Tier3.suite
  _ <- render (comment (div "TEST" "RELEASE-01/TIER-02" "Control.Tier2"))            *> wip
  _ <- render (comment (div "TEST" "RELEASE-01/TIER-01" "Control.Tier1"))            *> wip
  pure unit
  where 
    wip        = pure unit 
    render     = liftEffect <<< log
    div        = \x y z -> intercalate " " $ [h1 x, h2 z, h3 y]
    h1         = bold <<< fgYellow
    h2         = bold <<< fgCyan
    h3         = dim  <<< fgWhite
    comment    = dim <<<  fgWhite
    square     = \x ->  (fgGreen "[") <> x <>  (fgGreen "]")
    round      = \x ->  (fgGreen "(") <> x <>  (fgGreen ")") 
    dim        = \x -> "\x1b[2m" <> x <> "\x1b[0m" 
    bold       = \x -> "\x1b[1m" <> x <> "\x1b[0m" 
    fgCyan     = \x -> "\x1b[36m" <> x <> "\x1b[0m" 
    fgGreen    = \x -> "\x1b[32m" <> x <> "\x1b[0m" 
    fgYellow   = \x -> "\x1b[33m" <> x <> "\x1b[0m" 
    fgWhite    = \x -> "\x1b[37m" <> x <> "\x1b[0m" 
    fgMagenta  = \x -> "\x1b[35m" <> x <> "\x1b[0m" 
    underline  = \x -> "\x1b[4m" <> x <> "\x1b[0m" 
    {-- Reference: https://stackoverflow.com/questions/9781218/how-to-change-node-jss-console-font-color --}
