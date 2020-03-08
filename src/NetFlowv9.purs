module NetFlowv9
  ( RawHeader
  , RawFlowSet
  , readRawHeader
  , readRawFlowSets
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Data.Tuple(Tuple(..))

import Effect (Effect)
import Effect.Exception (Error)
import Effect.Exception as Exception

import Buffer as Buffer

type RawHeader =
  { version         :: Int
  , count           :: Int
  , systemUptime    :: Int
  , unixSeconds     :: Int
  , packageSequence :: Int
  , sourceID        :: Int
  }

type RawFlowSet =
  { flowSetID :: Int
  , length    :: Int
  , contents  :: Array Int
  }

readInt16BE' :: Int -> Array Int -> Effect (Either Error Int)
readInt16BE' x y = do
  buffer <- Buffer.from $ Array.slice x (x + 2) y
  result <- Buffer.readInt16BE buffer
  pure $ result

readInt32BE' :: Int -> Array Int -> Effect (Either Error Int)
readInt32BE' x y = do
  buffer <- Buffer.from $ Array.slice x (x + 4) y
  result <- Buffer.readInt32BE buffer
  pure $ result

readRawHeader :: Array Int -> Effect (Either Error (Tuple RawHeader (Array Int)))
readRawHeader packet = do
  u'     <- readInt16BE' 0  $ packet
  v'     <- readInt16BE' 2  $ packet
  w'     <- readInt32BE' 4  $ packet
  x'     <- readInt32BE' 8  $ packet
  y'     <- readInt32BE' 12 $ packet
  z'     <- readInt32BE' 16 $ packet
  result <- pure $ sequence [u',v',w',x',y',z']
  case result of
    (Left error)          -> pure $ Left error
    (Right [u,v,w,x,y,z]) -> do
      header <- pure $
        { version         : u
        , count           : v
        , systemUptime    : w
        , unixSeconds     : x
        , packageSequence : y
        , sourceID        : z
        }
      length <- pure $ Array.length packet
      body   <- pure $ Array.slice 20 length $  packet
      pure $ Right (Tuple header body)
    (Right _)             -> pure $ Left (Exception.error "Unexpected Result")

readRawFlowSet :: Array Int -> Effect (Either Error (Tuple RawFlowSet (Array Int)))
readRawFlowSet body = do
  x' <- readInt16BE' 0 $ body
  y' <- readInt16BE' 2 $ body
  result <- pure $ sequence [x',y'] 
  case result of
    (Left error)  -> pure $ Left error
    (Right [x,y]) -> do
      z       <- pure $ Array.slice 4 y $ body
      flowSet <- pure $
        { flowSetID : x
        , length    : y 
        , contents  : z 
        }
      length  <- pure $ Array.length body
      body'   <- pure $ Array.slice y length $ body
      pure $ Right (Tuple flowSet body')
    (Right _) -> pure $ Left (Exception.error "Unexpected result.")

readRawFlowSets' :: Array RawFlowSet -> Array Int -> Effect (Either Error (Array RawFlowSet))
readRawFlowSets' acc body = do
  result <- readRawFlowSet body
  case result of
    (Left error)                  -> pure $ Left error
    (Right (Tuple flowSet []))    -> pure $ Right (acc <> [flowSet]) 
    (Right (Tuple flowSet body')) -> readRawFlowSets' (acc <> [flowSet]) body'

readRawFlowSets :: Array Int -> Effect (Either Error (Array RawFlowSet))
readRawFlowSets = readRawFlowSets' []
