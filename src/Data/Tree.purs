module Data.Tree
  ( Tree(..)
  ) where

data Tree f a b c =
  Partition (f (Tree f a b c)) (f (Tree f a b c)) 
  | Index a (f (Tree f a b c))
  | Single b c
