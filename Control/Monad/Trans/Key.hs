{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Key (Key, newKey, Keyring, unKeyring, KeyringT, unKeyringT, lift) where

import Control.Applicative
import Control.Monad (MonadPlus (..), guard)
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.IORef
import Data.Type.Equality
import Numeric.Natural
import System.IO.Unsafe
import Unsafe.Coerce

newtype Key s a = Key Natural

instance TestEquality (Key s) where
    Key i `testEquality` Key j = unsafeCoerce Refl <$ guard (i == j)

newKey :: Applicative p => KeyringT s p (Key s a)
newKey = KeyringT . ReaderT $ \ r -> pure . unsafePerformIO . atomicModifyIORef' r $ liftA2 (,) (+1) Key

type Keyring s = KeyringT s Identity

unKeyring :: (∀ s . Keyring s a) -> a
unKeyring x = runIdentity (unKeyringT x)

newtype KeyringT s f a = KeyringT (ReaderT (IORef Natural) f a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadFail, MonadTrans)

lift :: f a -> KeyringT s f a
lift = KeyringT . ReaderT . pure

unKeyringT :: (∀ s . KeyringT s f a) -> f a
unKeyringT (KeyringT (ReaderT f)) = f $ unsafePerformIO $ newIORef 0
