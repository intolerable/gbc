module Memory where

import Control.Lens
import Data.Array
import Data.Bits
import Data.Default

import Types
import Utilities

newtype Memory = Memory { unMemory :: Array Address Byte }
  deriving (Show, Read, Eq)

instance Default Memory where
  def = emptyMemory

emptyMemory :: Memory
emptyMemory = Memory $ listArray (minBound, maxBound) $ repeat 0x0000

writeByte :: Address -> Byte -> Memory -> Memory
writeByte addr byte (Memory mem) =
  Memory $ mem & ix addr .~ byte

writeWord :: Address -> Word -> Memory -> Memory
writeWord addr word (Memory mem) =
  let (lower, upper) = word ^. wordPair in
  Memory $ mem & ix addr .~ lower
               & ix (addr + 1) .~ upper

readByte :: Address -> Memory -> Byte
readByte addr (Memory mem) = mem ^. singular (ix addr)

readWord :: Address -> Memory -> Word
readWord addr (Memory mem) = (get addr `shiftL` 8) + get (succ addr)
  where get a = fromIntegral $ mem ^. singular (ix a)
