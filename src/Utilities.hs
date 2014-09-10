module Utilities
  ( (~$$~)
  , wordPair ) where

import Control.Lens
import Control.Monad.State
import Data.Bits
import Data.Word

(~$$~) :: Lens' a b -> State b c -> State a c
(~$$~) l s = do
  initial <- use l
  let (res, new) = runState s initial
  l .= new
  return res

byteToWord :: (Word8, Word8) -> Word16
byteToWord (x, y) =
  (fromIntegral x `shiftL` 8) + fromIntegral y

wordToByte :: Word16 -> (Word8, Word8)
wordToByte x =
  (fromIntegral x, fromIntegral $ x `shiftR` 8)

wordPair :: Iso' Word16 (Word8, Word8)
wordPair = iso wordToByte byteToWord
