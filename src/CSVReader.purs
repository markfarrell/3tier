module CSVReader
  ( Options
  , writable
  ) where

import Effect (Effect)

import Stream (Writable) as Stream

type Options =
  { delimiter :: String
  , multiline :: Boolean
  , allowQuotes :: Boolean
  , skipEmptyLines :: Boolean
  , skipHeader :: Boolean
  }

foreign import writable :: Options -> Effect Stream.Writable
  
