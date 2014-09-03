module Utilities where

import Control.Lens
import Control.Monad.State

(~$$~) :: Lens' a b -> State b c -> State a c
(~$$~) l s = do
  initial <- use l
  let (res, new) = runState s initial
  l .= new
  return res
