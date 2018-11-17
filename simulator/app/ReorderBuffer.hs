module ReorderBuffer where

import Lib
import Data.Word
import Data.Maybe
import Data.Bits
import Control.Applicative
import qualified Data.Vector as V
import Debug.Trace
import qualified Data.Map.Strict as Map
import Utils
import Data.List
import Data.Ord

insertReorderBuffer :: ROBId -> Instruction -> Word32 -> ReorderBuffer -> ReorderBuffer
insertReorderBuffer robId instrct value reorderBuff =
     