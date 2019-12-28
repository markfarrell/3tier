module Client.InsertWindows
  where

import Prelude

import Effect (Effect)

import Process as Process
import Readline as Readline

import Client as Client

main :: Effect Unit
main = do
  interface <- Readline.createInterface Process.stdin Process.stdout false
  Client.launchProcess url' interface
  where url' = "http://0.0.0.0:3000/insert/windows?entry="
