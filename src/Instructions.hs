module Instructions where

import CPU
import qualified Memory
import Types

import Control.Applicative
import Control.Lens
import Control.Monad.State

nop :: State Z80 ()
nop = do
  registers.m .= 1
  registers.t .= 4

push :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
push r1 r2 = do
  registers.sp -= 1
  writeByte (registers.sp) (registers.r1)
  registers.sp -= 1
  writeByte (registers.sp) (registers.r2)
  registers.m .= 3; registers.t .= 12

pushbc :: State Z80 ()
pushbc = push b c

pushde :: State Z80 ()
pushde = push d e

pushhl :: State Z80 ()
pushhl = push h l

pushaf :: State Z80 ()
pushaf = push a (f.flags)

pop :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
pop l1 l2 = do
  registers.l1 <~ readByte (registers.sp)
  registers.sp += 1
  registers.l2 <~ readByte (registers.sp)
  registers.sp += 1
  registers.m .= 3; registers.t .= 12

popbc :: State Z80 ()
popbc = pop b c

popde :: State Z80 ()
popde = pop d e

pophl :: State Z80 ()
pophl = pop h l

popaf :: State Z80 ()
popaf = pop a (f.flags)

writeByte :: Lens' Z80 Address -> Lens' Z80 Byte -> State Z80 ()
writeByte addr byte =
  memory <~ Memory.writeByte <$> use addr <*> use byte <*> use memory

readByte :: Lens' Z80 Address -> State Z80 Byte
readByte addr =
  Memory.readByte <$> use addr <*> use memory
