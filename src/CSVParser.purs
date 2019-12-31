module CSVParser
  ( Options
  , createReader
  ) where

import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce, emit)

import Effect (Effect)
import Effect.Aff (Aff)

import Foreign (Foreign)

import Stream as Stream

type Options =
  { headers :: Array String
  }

foreign import createWritable :: Options -> Effect Stream.Writable

foreign import onRow :: (Foreign -> Effect Unit) -> Stream.Writable -> Effect Unit

reader :: Stream.Writable -> Producer Foreign Aff Unit
reader writable = produce \emitter -> do
  onRow (\row -> emit emitter $ row) $ writable

createReader :: Stream.Readable -> Options -> Effect (Producer Foreign Aff Unit)
createReader readable = \options -> do
  writable  <- createWritable options
  writable' <- Stream.pipe readable writable
  pure $ reader writable'
