{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Key (Keyring, Key, newKey, unKeyring) where

import Control.Applicative
import Control.Monad (guard)
import Data.IORef
import Data.Type.Equality
import Numeric.Natural
import System.IO.Unsafe
import Unsafe.Coerce

newtype Keyring s a = Keyring (IORef Natural -> a) deriving (Functor, Applicative, Monad)
newtype Key s a = Key Natural

instance TestEquality (Key s) where
    Key i `testEquality` Key j = unsafeCoerce Refl <$ guard (i == j)

newKey :: Keyring s (Key s a)
newKey = Keyring $ \ r -> unsafePerformIO . atomicModifyIORef' r $ liftA2 (,) (+1) Key

unKeyring :: (∀ s . Keyring s a) -> a
unKeyring (Keyring f) = f $ unsafePerformIO $ newIORef 0
