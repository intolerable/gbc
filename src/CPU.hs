module CPU where

import Memory
import Types
import Utilities

import Data.Bits
import Control.Lens
import Data.Default

data Z80 = Z80 { _z80Clock :: Clock
               , _z80Registers :: Registers
               , _z80Memory :: Memory }
  deriving (Show, Read, Eq)

instance Default Z80 where
  def = Z80 def def def

data Clock = Clock { _clockM :: Integer
                   , _clockT :: Integer }
  deriving (Show, Read, Eq)

instance Default Clock where
  def = Clock def def

data Registers = Registers { _registersA :: Byte
                           , _registersB :: Byte
                           , _registersC :: Byte
                           , _registersD :: Byte
                           , _registersE :: Byte
                           , _registersH :: Byte
                           , _registersL :: Byte
                           , _registersF :: Flags
                           , _registersPc :: Address
                           , _registersSp :: Address
                           , _registersM :: Integer
                           , _registersT :: Integer }
  deriving (Show, Read, Eq)

instance Default Registers where
  def = Registers def def def def
                  def def def def
                  def def def def

af :: Lens' Registers Address
af = lens
  (\ r -> (_registersA r, _registersF r ^. flags) ^. from wordPair)
  (\ r w ->
    let (a, f) = w ^. wordPair in r { _registersA = a, _registersF = f ^. from flags})

bc :: Lens' Registers Address
bc = lens
  (\ r -> (_registersB r, _registersC r) ^. from wordPair)
  (\ r w ->
    let (b, c) = w ^. wordPair in r { _registersB = b, _registersC = c})

de :: Lens' Registers Address
de = lens
  (\ r -> (_registersD r, _registersE r) ^. from wordPair)
  (\ r w ->
    let (d, e) = w ^. wordPair in r { _registersD = d, _registersE = e})

hl :: Lens' Registers Address
hl = lens
  (\ r -> (_registersH r, _registersL r) ^. from wordPair)
  (\ r w ->
    let (h, l) = w ^. wordPair in r { _registersH = h, _registersL = l})

data Flags = Flags { _flagsZero :: Bool
                   , _flagsSubtraction :: Bool
                   , _flagsHalfCarry :: Bool
                   , _flagsCarry :: Bool }
  deriving (Show, Read, Eq)

instance Default Flags where
  def = Flags False False False False

fromFlags :: Flags -> Byte
fromFlags f = zeroBits & 7 `setIf` _flagsZero f
                       & 6 `setIf` _flagsSubtraction f
                       & 5 `setIf` _flagsHalfCarry f
                       & 4 `setIf` _flagsCarry f
  where setIf :: Int -> Bool -> Byte -> Byte
        setIf n i b = if i then b `setBit` n else b

toFlags :: Byte -> Flags
toFlags byte = Flags (byte `testBit` 7)
                     (byte `testBit` 6)
                     (byte `testBit` 5)
                     (byte `testBit` 4)

flags :: Iso' Flags Byte
flags = iso fromFlags toFlags

makeFields ''Z80
makeFields ''Clock
makeFields ''Registers
makeFields ''Flags
