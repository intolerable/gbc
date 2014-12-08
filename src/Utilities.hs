module Utilities
  ( wordPair ) where

import Control.Lens
import Data.Bits
import Data.Word

byteToWord :: (Word8, Word8) -> Word16
byteToWord (x, y) =
  (fromIntegral x `shiftL` 8) + fromIntegral y

wordToByte :: Word16 -> (Word8, Word8)
wordToByte x =
  (fromIntegral x, fromIntegral $ x `shiftR` 8)

wordPair :: Iso' Word16 (Word8, Word8)
wordPair = iso wordToByte byteToWord
