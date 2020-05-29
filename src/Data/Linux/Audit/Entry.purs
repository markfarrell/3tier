module Data.Linux.Audit.Entry
  ( Entry(..)
  , Key
  , Name
  , Value
  ) where

import Data.NonEmpty (NonEmpty)

import Data.Tree (Tree)

type Key   = String

type Name  = String

type Value = String

type Entry = Tree (NonEmpty Array) Key Name Value
