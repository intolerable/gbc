module Types where

import Data.IORef
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Data.Word

type Byte = Word8
type Address = Word16
type Word = Word16

newtype IOStateT s m a = IOStateT { runIOState :: ReaderT (IORef s) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

type IOState s a = IOStateT s IO a

instance MonadIO m => MonadState s (IOStateT s m) where
  get = do
    ref <- IOStateT ask
    liftIO $ readIORef ref
  put s = do
    ref <- IOStateT ask
    liftIO $ writeIORef ref s
