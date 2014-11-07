module Instructions where

import CPU
import qualified Memory
import Types

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Default

nop :: State Z80 ()
nop = do
  registers.m .= 1
  registers.t .= 4

add :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
add r1 r2 = do
  r1' <- asInteger $ use (registers.r1)
  r2' <- asInteger $ use (registers.r2)
  let res = r1' + r2'
  registers.f .= (def & zero .~ (res == 0)
                      & carry .~ (res > 255))
  registers.r1 .= fromIntegral res
  registers.m .= 1; registers.t .= 4
  where asInteger = fmap (fromIntegral :: Integral a => a -> Integer)

ldr :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
ldr r1 r2 = do
  registers.r1 <~ use (registers.r2)
  registers.m .= 1; registers.t .= 4

ldrhlm :: Lens' Registers Byte -> State Z80 ()
ldrhlm r = do
  registers.r <~ Memory.readByte <$> use (registers.hl) <*> use memory
  registers.m .= 2; registers.t .= 8

swap :: Lens' Registers Byte -> State Z80 ()
swap r = do
  res <- use (registers.r)
  registers.r <~ readByte (registers.hl)
  memory <~ Memory.writeByte <$> use (registers.hl) <*> pure res <*> use memory
  registers.m .= 4; registers.t .= 16

push :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
push r1 r2 = do
  registers.sp -= 1
  writeByte (registers.sp) (registers.r1)
  registers.sp -= 1
  writeByte (registers.sp) (registers.r2)
  registers.m .= 3; registers.t .= 12

pop :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
pop l1 l2 = do
  registers.l1 <~ readByte (registers.sp)
  registers.sp += 1
  registers.l2 <~ readByte (registers.sp)
  registers.sp += 1
  registers.m .= 3; registers.t .= 12

writeByte :: Lens' Z80 Address -> Lens' Z80 Byte -> State Z80 ()
writeByte addr byte =
  memory <~ Memory.writeByte <$> use addr <*> use byte <*> use memory

readByte :: Lens' Z80 Address -> State Z80 Byte
readByte addr =
  Memory.readByte <$> use addr <*> use memory

writeWord :: Lens' Z80 Address -> Lens' Z80 Address -> State Z80 ()
writeWord addr word =
  memory <~ Memory.writeWord <$> use addr <*> use word <*> use memory

readWord :: Lens' Z80 Address  -> State Z80 Address
readWord addr =
  Memory.readWord <$> use addr <*> use memory

ops :: [State Z80 ()]
ops =
  -- 0x00
  [ nop, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x10
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x20
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x30
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x40
  , ldr b b, ldr b c, ldr b d, ldr b e
  , ldr b h, ldr b l, ldrhlm b, ldr a a
  , ldr c b, ldr c c, ldr c d, ldr c e
  , ldr c h, ldr c l, ldrhlm c, ldr c a

  -- 0x50
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x60
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x70
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x80
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x90
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xA0
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xB0
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xC0
  , undefined, undefined, undefined, undefined
  , undefined, push b c, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xD0
  , undefined, undefined, undefined, undefined
  , undefined, push d e, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xE0
  , undefined, undefined, undefined, undefined
  , undefined, push h l, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xF0
  , undefined, undefined, undefined, undefined
  , undefined, push a (f.flags), undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  ]
