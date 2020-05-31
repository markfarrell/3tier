module Foreign.Class
  ( class Marshall
  , marshall
  ) where

import Prelude

import Data.Tuple (Tuple)

import Foreign (Foreign)
import Foreign.Coerce (coerce) as F
import Foreign.Object (fromArray) as F

import FFI.Date (Date)
import FFI.UUID (UUID)

class Marshall a where
  marshall :: a -> Foreign

instance marshallInt :: Marshall Int where
  marshall = F.coerce <<< show

instance marshallDate :: Marshall Date where
  marshall = F.coerce <<< show

instance marshallUUID :: Marshall UUID where
  marshall = F.coerce <<< show

instance marshallString :: Marshall String where
  marshall = F.coerce

instance marshallFromArray :: Marshall (Array (Tuple String Foreign)) where
  marshall = F.fromArray
