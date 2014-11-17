module Instructions (ops, cbops) where

import CPU
import qualified Memory
import Types
import Utilities

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Default

nop :: State Z80 ()
nop = registers ~$$~ do
  m .= 1; t .= 4

add :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
add r1 r2 = do
  r1' <- fromIntegral <$> use (registers.r1)
  r2' <- fromIntegral <$> use (registers.r2)
  let res = r1' + r2' :: Integer
  registers.f .= (def & zero .~ (res == 0)
                      & carry .~ (res > 255))
  registers.r1 .= fromIntegral res
  registers.m .= 1; registers.t .= 4

inc :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
inc r1 r2 = registers ~$$~ do
  res <- r2 <+= 1
  when (res == 0) $ r1 += 1
  m .= 1; t .= 4

incsp :: State Z80 ()
incsp = registers ~$$~ do
  sp += 1
  m .= 1; t .= 4

ldr :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
ldr r1 r2 = registers ~$$~ do
  r1 <~ use r2
  m .= 1; t .= 4

ldrhlm :: Lens' Registers Byte -> State Z80 ()
ldrhlm r = do
  registers.r <~ readByte (registers.hl)
  registers.m .= 2; registers.t .= 8

ldnn :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
ldnn r1 r2 = do
  registers.r2 <~ readByte (registers.pc)
  registers.pc += 1
  registers.r1 <~ readByte (registers.pc)
  registers.pc += 1
  registers.m .= 3; registers.t .= 12

ldma :: Lens' Registers Byte -> Lens' Registers Byte -> State Z80 ()
ldma r1 r2 = do
  res1 <- use $ registers.r1
  res2 <- use $ registers.r2
  memory <~ Memory.writeByte <$> pure ((res1, res2) ^. from wordPair) <*> use (registers.a) <*> use memory

ldnnsp :: State Z80 ()
ldnnsp = do
  registers.sp <~ readWord (registers.pc)
  registers.pc += 2
  registers.m .= 3; registers.t .= 12

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
  [ nop, ldnn b c, ldma b c, inc b c
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x10
  , undefined, ldnn d e, ldma d e, inc d e
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x20
  , undefined, ldnn h l, undefined, inc h l
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0x30
  , undefined, ldnnsp, undefined, incsp
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
  , add a b, add a c, add a d, add a e
  , add a h, add a l, undefined, add a a
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
  , undefined, pop b c, undefined, undefined
  , undefined, push b c, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xD0
  , undefined, pop d e, undefined, undefined
  , undefined, push d e, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xE0
  , undefined, pop h l, undefined, undefined
  , undefined, push h l, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xF0
  , undefined, pop a (f.flags), undefined, undefined
  , undefined, push a (f.flags), undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  ]

cbops :: [State Z80 ()]
cbops =
  -- 0xCB00
  [ undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCB10
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCB20
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCB30
  , swap b, swap c, swap d, swap e
  , swap h, swap l, undefined, swap a
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCB40
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCB50
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCB60
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCB70
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCB80
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCB90
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCBA0
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCBB0
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCBC0
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCBD0
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCBE0
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined

  -- 0xCBF0
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined
  , undefined, undefined, undefined, undefined ]
