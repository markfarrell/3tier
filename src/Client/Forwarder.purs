module Client.Forwarder
  where

import Prelude

import Effect (Effect)

import Data.Array as Array

import Process as Process
import Readline as Readline

import Client as Client

main :: Effect Unit
main = do
  interface <- Readline.createInterface Process.stdin Process.stdout false
  case argv' of
    [url'] -> Client.launchProcess url' interface
    _      -> pure unit
    where argv'  = Array.drop 2 Process.argv
