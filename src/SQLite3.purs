module SQLite3
  ( Mode (..)
  , Database
  , Row
  , connect
  , close
  , all
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

data Mode = OpenReadOnly | OpenCreate | OpenReadWrite

instance showMode :: Show Mode where
  show OpenReadOnly = "OPEN_READONLY"
  show OpenCreate = "OPEN_CREATE"
  show OpenReadWrite = "OPEN_READWRITE"

mode' :: Mode -> Int
mode' OpenReadOnly = 1
mode' OpenCreate = 4
mode' OpenReadWrite = 2 

foreign import data Database :: Type
foreign import data Row :: Type

foreign import connectImpl :: String -> Int -> EffectFnAff Database

foreign import closeImpl :: Database -> EffectFnAff Unit

foreign import allImpl :: String -> Database -> EffectFnAff (Array Row)

foreign import showRowImpl :: Row -> String

connect :: String -> Mode -> Aff Database
connect filename mode = fromEffectFnAff $ connectImpl filename (mode' mode)

close :: Database -> Aff Unit
close = fromEffectFnAff <<< closeImpl

all :: String -> Database -> Aff (Array Row)
all query db = fromEffectFnAff $ allImpl query db

instance showDatabase :: Show Database where
  show _ = "Database"

instance showRow :: Show Row where
  show = showRowImpl
