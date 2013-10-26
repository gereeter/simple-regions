{-# LANGUAGE RankNTypes #-}

module Control.Monad.Trans.Region (
      RegionT
    , runRegionT
    , addFinalizer
) where

import Control.Applicative (Applicative(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))

import Control.Monad (ap, liftM)

import Control.Monad.Safe

newtype RegionT s m a = RegionT {unRegionT :: m (a, m ())}

instance Monad m => Functor (RegionT s m) where
    fmap f (RegionT act) = RegionT (liftM (\(a, finalize) -> (f a, finalize)) act)

instance MonadSafe m => Applicative (RegionT s m) where
    pure = return
    (<*>) = ap

instance MonadSafe m => Monad (RegionT s m) where
    return x = RegionT (return (x, return ()))
    m >>= f = RegionT $ do
        (m', release1) <- unRegionT m
        (x , release2) <- unRegionT (f m') `onException` release1
        return (x, release2 >> release1) -- FIXME: Should I use 'finally' to compose the finalizers?

instance MonadTrans (RegionT s) where
    lift mx = RegionT $ do
        x <- mx
        return (x, return ())

instance (MonadIO m, MonadSafe m) => MonadIO (RegionT s m) where
    liftIO = lift . liftIO

instance MonadSafe m => MonadSafe (RegionT s m) where
    RegionT act `onException` handle = RegionT (act `onException` unsafeRunRegionT handle)

addFinalizer :: Monad m => m () -> RegionT s m ()
addFinalizer close = RegionT (return ((), close))

runRegionT :: Monad m => (forall s . RegionT s m a) -> m a
runRegionT act = unsafeRunRegionT act

unsafeRunRegionT :: Monad m => RegionT s m a -> m a
unsafeRunRegionT (RegionT act) = do
    (x, release) <- act
    release
    return x
