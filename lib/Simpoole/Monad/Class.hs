{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simpoole.Monad.Class (MonadPool (..)) where

import qualified Control.Monad.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Lazy as State.Lazy
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Writer.Strict as Writer

-- | A pooled resource is available through @m@
--
-- @since tbd
class MonadPool resource m where
  -- | Grab a resource and do something with it.
  --
  -- @since tbd
  withResource :: (resource -> m a) -> m a

instance MonadPool resource m => MonadPool resource (State.StateT s m) where
  withResource f = State.StateT $ \state ->
    withResource $ \resource -> State.runStateT (f resource) state

  {-# INLINE withResource #-}

instance MonadPool resource m => MonadPool resource (State.Lazy.StateT s m) where
  withResource f = State.Lazy.StateT $ \state ->
    withResource $ \resource -> State.Lazy.runStateT (f resource) state

  {-# INLINE withResource #-}

instance MonadPool resource m => MonadPool resource (Writer.WriterT w m) where
  withResource f = Writer.WriterT $
    withResource $ \resource -> Writer.runWriterT $ f resource

  {-# INLINE withResource #-}

instance MonadPool resource m => MonadPool resource (Writer.Lazy.WriterT w m) where
  withResource f = Writer.Lazy.WriterT $
    withResource $ \resource -> Writer.Lazy.runWriterT $ f resource

  {-# INLINE withResource #-}

instance MonadPool resource m => MonadPool resource (Reader.ReaderT r m) where
  withResource f = Reader.ReaderT $ \state ->
    withResource $ \resource -> Reader.runReaderT (f resource) state

  {-# INLINE withResource #-}

instance MonadPool resource m => MonadPool resource (RWS.RWST r s w m) where
  withResource f = RWS.RWST $ \env state ->
    withResource $ \resource -> RWS.runRWST (f resource) env state

  {-# INLINE withResource #-}

instance MonadPool resource m => MonadPool resource (RWS.Lazy.RWST r s w m) where
  withResource f = RWS.Lazy.RWST $ \env state ->
    withResource $ \resource -> RWS.Lazy.runRWST (f resource) env state

  {-# INLINE withResource #-}
