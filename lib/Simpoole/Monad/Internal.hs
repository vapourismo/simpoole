{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide #-}

module Simpoole.Monad.Internal
  ( PoolEnv (..)
  , MonadPool (..)
  , PoolT (..)
  , runPoolT
  , hoistPoolT
  )
where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Conc.Class as Conc
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Lazy as State.Lazy
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Writer.Class (MonadWriter)
import qualified Control.Monad.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Writer.Strict as Writer
import           Data.Proxy (Proxy (Proxy))
import qualified Simpoole as Pool

data PoolEnv m resource = PoolEnv
  { poolEnv_resource :: Maybe resource
  , poolEnv_pool :: Pool.Pool m resource
  }

-- | Monad transformer for operations on pools
--
-- This transformer can help you if you have problems with re-entrance (e.g. nested
-- 'Pool.withResource' calls).
--
-- > withResource $ \x -> withResource $ \y -> ...
--
-- In the above example @x@ and @y@ are the same resource.
--
-- Note, this does not apply when spawning new threads in the outer 'withResource' scope using
-- 'Conc.MonadConc'.
--
-- > withResource $ \x -> async $ withResource $ \y -> ...
--
-- In the special case above, @x@ and @y@ are not the same resource because the closure given to
-- 'async' does not inherit the associated resource from the outer 'withResource' closure.
--
-- @since tbd
newtype PoolT resource m a = PoolT
  { unPoolT :: Reader.ReaderT (PoolEnv m resource) m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadIO
    , Catch.MonadThrow
    , Catch.MonadCatch
    , Catch.MonadMask
    , MonadState s
    , MonadError e
    , MonadWriter w
    )

instance MonadTrans (PoolT resource) where
  lift = PoolT . Reader.ReaderT . const

  {-# INLINE lift #-}

instance Reader.MonadReader r m => Reader.MonadReader r (PoolT resource m) where
  ask = lift Reader.ask

  {-# INLINE ask #-}

  local f (PoolT inner) = PoolT (Reader.mapReaderT (Reader.local f) inner)

  {-# INLINE local #-}

  reader f = lift (Reader.reader f)

  {-# INLINE reader #-}

instance Conc.MonadConc m => Conc.MonadConc (PoolT resource m) where
  type STM (PoolT resource m) = Conc.STM m

  type MVar (PoolT resource m) = Conc.MVar m

  type IORef (PoolT resource m) = Conc.IORef m

  type Ticket (PoolT resource m) = Conc.Ticket m

  type ThreadId (PoolT resource m) = Conc.ThreadId m

  forkWithUnmask f = PoolT $ Reader.ReaderT $ \PoolEnv {poolEnv_pool} ->
    Conc.forkWithUnmask $ \unmask ->
      runPoolT poolEnv_pool (f (hoistPoolT unmask))

  {-# INLINE forkWithUnmask #-}

  forkWithUnmaskN name f = PoolT $ Reader.ReaderT $ \PoolEnv {poolEnv_pool} ->
    Conc.forkWithUnmaskN name $ \unmask ->
      runPoolT poolEnv_pool (f (hoistPoolT unmask))

  {-# INLINE forkWithUnmaskN #-}

  forkOnWithUnmask num f = PoolT $ Reader.ReaderT $ \PoolEnv {poolEnv_pool} ->
    Conc.forkOnWithUnmask num $ \unmask ->
      runPoolT poolEnv_pool (f (hoistPoolT unmask))

  {-# INLINE forkOnWithUnmask #-}

  forkOnWithUnmaskN name num f = PoolT $ Reader.ReaderT $ \PoolEnv {poolEnv_pool} ->
    Conc.forkOnWithUnmaskN name num $ \unmask ->
      runPoolT poolEnv_pool (f (hoistPoolT unmask))

  {-# INLINE forkOnWithUnmaskN #-}

  forkOSWithUnmask f = PoolT $ Reader.ReaderT $ \PoolEnv {poolEnv_pool} ->
    Conc.forkOSWithUnmask $ \unmask ->
      runPoolT poolEnv_pool (f (hoistPoolT unmask))

  {-# INLINE forkOSWithUnmask #-}

  forkOSWithUnmaskN name f = PoolT $ Reader.ReaderT $ \PoolEnv {poolEnv_pool} ->
    Conc.forkOSWithUnmaskN name $ \unmask ->
      runPoolT poolEnv_pool (f (hoistPoolT unmask))

  {-# INLINE forkOSWithUnmaskN #-}

  supportsBoundThreads = lift Conc.supportsBoundThreads

  {-# INLINE supportsBoundThreads #-}

  isCurrentThreadBound = lift Conc.isCurrentThreadBound

  {-# INLINE isCurrentThreadBound #-}

  getNumCapabilities = lift Conc.getNumCapabilities

  {-# INLINE getNumCapabilities #-}

  setNumCapabilities x = lift (Conc.setNumCapabilities x)

  {-# INLINE setNumCapabilities #-}

  myThreadId = lift Conc.myThreadId

  {-# INLINE myThreadId #-}

  yield = lift Conc.yield

  {-# INLINE yield #-}

  threadDelay x = lift (Conc.threadDelay x)

  {-# INLINE threadDelay #-}

  newEmptyMVar = lift Conc.newEmptyMVar

  {-# INLINE newEmptyMVar #-}

  newEmptyMVarN x = lift (Conc.newEmptyMVarN x)

  {-# INLINE newEmptyMVarN #-}

  putMVar x y = lift (Conc.putMVar x y)

  {-# INLINE putMVar #-}

  tryPutMVar x y = lift (Conc.tryPutMVar x y)

  {-# INLINE tryPutMVar #-}

  readMVar x = lift (Conc.readMVar x)

  {-# INLINE readMVar #-}

  tryReadMVar x = lift (Conc.tryReadMVar x)

  {-# INLINE tryReadMVar #-}

  takeMVar x = lift (Conc.takeMVar x)

  {-# INLINE takeMVar #-}

  tryTakeMVar x = lift (Conc.tryTakeMVar x)

  {-# INLINE tryTakeMVar #-}

  newIORef x = lift (Conc.newIORef x)

  {-# INLINE newIORef #-}

  newIORefN x y = lift (Conc.newIORefN x y)

  {-# INLINE newIORefN #-}

  readIORef x = lift (Conc.readIORef x)

  {-# INLINE readIORef #-}

  atomicModifyIORef x y = lift (Conc.atomicModifyIORef x y)

  {-# INLINE atomicModifyIORef #-}

  writeIORef x y = lift (Conc.writeIORef x y)

  {-# INLINE writeIORef #-}

  atomicWriteIORef x y = lift (Conc.atomicWriteIORef x y)

  {-# INLINE atomicWriteIORef #-}

  readForCAS x = lift (Conc.readForCAS x)

  {-# INLINE readForCAS #-}

  peekTicket' _ y = Conc.peekTicket' @m Proxy y

  {-# INLINE peekTicket' #-}

  casIORef x y z = lift (Conc.casIORef x y z)

  {-# INLINE casIORef #-}

  modifyIORefCAS x y = lift (Conc.modifyIORefCAS x y)

  {-# INLINE modifyIORefCAS #-}

  modifyIORefCAS_ x y = lift (Conc.modifyIORefCAS_ x y)

  {-# INLINE modifyIORefCAS_ #-}

  atomically x = lift (Conc.atomically x)

  {-# INLINE atomically #-}

  newTVarConc x = lift (Conc.newTVarConc x)

  {-# INLINE newTVarConc #-}

  readTVarConc x = lift (Conc.readTVarConc x)

  {-# INLINE readTVarConc #-}

  throwTo x y = lift (Conc.throwTo x y)

  {-# INLINE throwTo #-}

  getMaskingState = lift Conc.getMaskingState

  {-# INLINE getMaskingState #-}

  unsafeUnmask = hoistPoolT Conc.unsafeUnmask

  {-# INLINE unsafeUnmask #-}

-- | Run the monad transformer against the given pool.
--
-- @since tbd
runPoolT :: Pool.Pool m resource -> PoolT resource m a -> m a
runPoolT pool (PoolT inner) =
  Reader.runReaderT inner PoolEnv
    { poolEnv_resource = Nothing
    , poolEnv_pool = pool
    }

{-# INLINE runPoolT #-}

-- | Lift an operation on the underlying functor.
--
-- @since tbd
hoistPoolT :: (m a -> m b) -> PoolT resource m a -> PoolT resource m b
hoistPoolT f action = PoolT $ Reader.ReaderT $ \env ->
  f (Reader.runReaderT (unPoolT action) env)

{-# INLINE hoistPoolT #-}

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

instance Catch.MonadMask m => MonadPool resource (PoolT resource m) where
  withResource f = PoolT $ Reader.ReaderT $ \poolEnv ->
    case poolEnv_resource poolEnv of
      Nothing ->
        Pool.withResource (poolEnv_pool poolEnv) $ \resource ->
          Reader.runReaderT (unPoolT (f resource)) poolEnv { poolEnv_resource = Just resource }

      Just resource ->
        Reader.runReaderT (unPoolT (f resource)) poolEnv

  {-# INLINE withResource #-}
