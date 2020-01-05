module CSVParser
  ( Options
  , createReader
  ) where

import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce, emit)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))

import Effect (Effect)
import Effect.Aff (Aff, launchAff)

import Foreign (Foreign)
import Foreign as Foreign

import Stream as Stream

import Audit as Audit

type Options =
  { separator :: String
  , headers :: Array String
  }

foreign import createWritable :: Options -> Effect Stream.Writable

foreign import onRow :: (Foreign -> Effect Unit) -> Stream.Writable -> Effect Unit

reader :: forall a. Show a => (Foreign -> Foreign.F a) -> Stream.Writable -> Producer a Aff Unit
reader read writable = produce \emitter -> do
  onRow (\row -> emit' emitter $ row) $ writable
  where
    emit' emitter = \row -> do
      result <- pure $ runExcept (read row)
      case result of 
        (Left error)     -> do
           _ <- debug $ Audit.Entry Audit.Failure Audit.DeserializationRequest (show error)
           pure unit
        (Right row')     -> do 
           _ <- debug $ Audit.Entry Audit.Success Audit.DeserializationRequest (show row') 
           emit emitter $ row'
    debug entry = void $ launchAff $ Audit.debug entry

createReader :: forall a. Show a => (Foreign -> Foreign.F a) -> Stream.Readable -> Options -> Effect (Producer a Aff Unit)
createReader read readable options = do
  writable  <- createWritable options
  writable' <- Stream.pipe readable writable
  pure $ reader read $ writable'