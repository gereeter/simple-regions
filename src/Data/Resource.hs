module Data.Resource (
      Resource
    , allocate
    , withResource
    , liftResource
    , mkResource
    , unsafeGetResource
) where

import Control.Monad.Safe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Region

newtype Resource s a = Resource a

allocate :: MonadSafe m => m a -> (a -> m ()) -> RegionT s m (Resource s a)
allocate alloc close = do
    x <- lift alloc
    addFinalizer (close x)
    return (Resource x)

liftResource :: MonadSafe m => Resource s a -> RegionT t (RegionT s m) (Resource t a)
liftResource (Resource a) = return (Resource a)

withResource :: Resource s a -> (a -> RegionT s m b) -> RegionT s m b
withResource (Resource a) f = f a

mkResource :: a -> Resource s a
mkResource = Resource

unsafeGetResource :: Resource s a -> a
unsafeGetResource (Resource a) = a
