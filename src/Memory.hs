module Memory where

import Control.Lens
import Data.Array
import Data.Bits
import Data.Default

import Types

newtype Memory = Memory { unMemory :: Array Address Byte }
  deriving (Show, Read, Eq)

instance Default Memory where
  def = emptyMemory

emptyMemory :: Memory
emptyMemory = Memory $ listArray (minBound, maxBound) $ repeat 0x0000

writeByte :: Address -> Byte -> Memory -> Memory
writeByte addr byte (Memory mem) = Memory $ mem & ix addr .~ byte

writeWord :: Address -> Word -> Memory -> Memory
writeWord addr word (Memory mem) = Memory $ mem & ix addr .~ fromIntegral word
                                                & ix (addr + 1) .~ fromIntegral (word `shiftR` 8)

readByte :: Address -> Memory -> Byte
readByte addr (Memory mem) = mem ^. singular (ix addr)

readWord :: Address -> Memory -> Word
readWord addr (Memory mem) = (get addr `shiftL` 8) + get (succ addr)
  where get a = fromIntegral $ mem ^. singular (ix a)
