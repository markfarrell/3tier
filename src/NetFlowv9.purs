module NetFlowv9
  ( Packet
  , Header
  , TemplateFlowSet
  , readPacket
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Data.Tuple(Tuple(..))

import Effect (Effect)
import Effect.Exception (Error)
import Effect.Exception as Exception

import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (sepBy)

import Arrays as Arrays
import Buffer as Buffer
import Parser as Parser

data Packet = Packet Header FlowSets

data Header = Header
  { version         :: Int
  , count           :: Int
  , systemUptime    :: Int
  , unixSeconds     :: Int
  , packageSequence :: Int
  , sourceID        :: Int
  }

type FlowSets = Array (Tuple (Array TemplateFlowSet) (Array DataFlowSet))

data TemplateFlowSet = TemplateFlowSet 
  { flowSetID :: Int
  , length :: Int
  , templates :: Array Template
  }

data Template = Template
  { templateID :: Int
  , fieldCount :: Int
  , fields     :: Array (Tuple Int Int)
  }

data DataFlowSet = DataFlowSet 
  { flowSetID :: Int
  , length :: Int
  , bytes :: Array Int
  }

instance showPacket :: Show Packet where
  show (Packet x y) = "(Packet " <> (Arrays.join " " [show x, show y]) <> ")"

instance showHeader :: Show Header where
  show (Header x) = "(Header " <> show x <> ")"

instance showTemplateFlowSet :: Show TemplateFlowSet where
  show (TemplateFlowSet x) = "(TemplateFlowSet " <> show x <> ")"

instance showDataFlowSet :: Show DataFlowSet where
  show (DataFlowSet x) = "(DataFlowSet " <> show x <> ")"


instance showTemplate :: Show Template where
  show (Template x) = "(Template " <> show x <> ")"

readInt16BE :: Int -> Array Int -> Effect (Either Error Int)
readInt16BE x y = do
  buffer <- Buffer.from $ Array.slice x (x + 2) y
  result <- Buffer.readInt16BE buffer
  pure $ result

readInt32BE :: Int -> Array Int -> Effect (Either Error Int)
readInt32BE x y = do
  buffer <- Buffer.from $ Array.slice x (x + 4) y
  result <- Buffer.readInt32BE buffer
  pure $ result

readInt16BEs :: Array Int -> Effect (Either Error (Array Int))
readInt16BEs array = do
  length   <- pure $ Array.length array
  indices  <- pure $ ((*) 2) <$>  Array.range 0 ((length - 1) / 2)
  results  <- sequence $ (flip readInt16BE array) <$> indices 
  pure $ sequence results

readHeader :: Array Int -> Effect (Either Error (Tuple Header (Array Int)))
readHeader packet = do
  u'     <- readInt16BE 0  $ packet
  v'     <- readInt16BE 2  $ packet
  w'     <- readInt32BE 4  $ packet
  x'     <- readInt32BE 8  $ packet
  y'     <- readInt32BE 12 $ packet
  z'     <- readInt32BE 16 $ packet
  result <- pure $ sequence [u',v',w',x',y',z']
  case result of
    (Left error)          -> pure $ Left error
    (Right [u,v,w,x,y,z]) -> do
      header <- pure $ Header
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

readTemplateFlowSet :: Array Int -> Effect (Either Error (Tuple TemplateFlowSet (Array Int)))
readTemplateFlowSet body = do
  x' <- readInt16BE 0 $ body
  y' <- readInt16BE 2 $ body
  result <- pure $ sequence [x',y'] 
  case result of
    (Left error)  -> pure $ Left error
    (Right [x,y]) -> do
      body'    <- pure $ Array.slice 4 y $ body
      flowSet' <- case x == 0 of
        true  -> do
          result' <- readInt16BEs body'
          case result' of
            (Left error')  -> pure $ Left error'
            (Right templates''') -> do 
              templates'' <- pure $ Arrays.join comma (show <$> templates''')
              case runParser templates'' templates of
                (Left _)          -> pure $ Left (Exception.error "Unexpected result (ParseError).")
                (Right templates') -> pure $ Right $ TemplateFlowSet $
                  { flowSetID    : x
                  , length       : y 
                  , templates    : templates' 
                  }
        false -> pure $ Left (Exception.error $ "Invalid flowset ID (" <> show x <> ")")
      length  <- pure $ Array.length body
      body''  <- pure $ Array.slice y length $ body
      case flowSet' of
        (Left error'')  -> pure $ Left error''
        (Right flowSet) -> pure $ Right (Tuple flowSet body'') 
    (Right _) -> pure $ Left (Exception.error "Unexpected result.")

readTemplateFlowSets' :: Array TemplateFlowSet -> Array Int -> Effect (Tuple (Array TemplateFlowSet) (Array Int))
readTemplateFlowSets' acc body = do
  result <- readTemplateFlowSet body
  case result of
    (Left error)                  -> pure $ Tuple acc body
    (Right (Tuple flowSet []))    -> pure $ Tuple (acc <> [flowSet]) [] 
    (Right (Tuple flowSet body')) -> readTemplateFlowSets' (acc <> [flowSet]) body'

readTemplateFlowSets :: Array Int -> Effect (Tuple (Array TemplateFlowSet) (Array Int))
readTemplateFlowSets = readTemplateFlowSets' []

readDataFlowSet :: Array Int -> Effect (Either Error (Tuple DataFlowSet (Array Int)))
readDataFlowSet body = do
  x' <- readInt16BE 0 $ body
  y' <- readInt16BE 2 $ body
  result <- pure $ sequence [x',y'] 
  case result of
    (Left error)  -> pure $ Left error
    (Right [x,y]) -> do
      body'    <- pure $ Array.slice 4 y $ body
      flowSet' <- case x > 0 of
        true  -> pure $ Right $ DataFlowSet
          { flowSetID : x
          , length    : y
          , bytes     : body'
          }
        false -> pure $ Left (Exception.error $ "Invalid flowset ID (" <> show x <> ")")
      length  <- pure $ Array.length body
      body''  <- pure $ Array.slice y length $ body
      case flowSet' of
        (Left error'')  -> pure $ Left error''
        (Right flowSet) -> pure $ Right (Tuple flowSet body'') 
    (Right _) -> pure $ Left (Exception.error "Unexpected result.")

readDataFlowSets' :: Array DataFlowSet -> Array Int -> Effect (Tuple (Array DataFlowSet) (Array Int))
readDataFlowSets' acc body = do
  result <- readDataFlowSet body
  case result of
    (Left error)                  -> pure $ Tuple acc body 
    (Right (Tuple flowSet []))    -> pure $ Tuple (acc <> [flowSet]) []
    (Right (Tuple flowSet body')) -> readDataFlowSets' (acc <> [flowSet]) body'

readDataFlowSets :: Array Int -> Effect (Tuple (Array DataFlowSet) (Array Int))
readDataFlowSets = readDataFlowSets' []

readFlowSets' :: FlowSets -> Array Int -> Effect FlowSets
readFlowSets' acc body = do
  templateFlowSets' <- readTemplateFlowSets body
  case templateFlowSets' of
    (Tuple templateFlowSets body') -> do
      dataFlowSets' <- readDataFlowSets body'
      case dataFlowSets' of
        (Tuple dataFlowSets body'') -> do
          acc'   <- pure $ acc <> [Tuple templateFlowSets dataFlowSets]
          case (Array.length body'' >= 4) of
            true  -> readFlowSets' acc' body''
            false -> pure acc'

readFlowSets :: Array Int -> Effect FlowSets
readFlowSets = readFlowSets' []

readPacket :: Array Int -> Effect (Either Error Packet)
readPacket packet = do
  header' <- readHeader packet
  case header' of
    (Left error)                -> pure $ Left error
    (Right (Tuple header body)) -> do
       flowSets <- readFlowSets body
       pure $ Right (Packet header flowSets)

template :: Parser String Template
template = do
  templateID <- Parser.positiveInteger
  _          <- separator
  fieldCount <- Parser.positiveInteger
  _          <- separator
  fields''   <- fields fieldCount
  pure $ Template $
    { templateID : templateID
    , fieldCount : fieldCount
    , fields     : fields''
    }
  where
    fields n = fields' [] 0 n
    fields' acc m n = case (m == n) of
      true  -> pure acc
      false -> do
        fieldType'  <- fieldType
        _           <- separator
        fieldLength <- Parser.positiveInteger
        _           <- separator' m n
        result <- fields' (acc <> [Tuple fieldType' fieldLength]) (m + 1) n
        pure result
    fieldType = do
      result <- Parser.positiveInteger
      case (result >= 0) && (result <= 127) of
        true  -> pure result
        false -> fail "Invalid field type." 
    separator' m n = case m < (n - 1) of
      true  -> do
        result <- separator
        pure result
      false -> pure ""

templates :: Parser String (Array Template)
templates = Array.fromFoldable <$> sepBy template separator

separator :: Parser String String
separator = string comma

comma :: String
comma = ","
