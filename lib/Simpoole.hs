{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Simpoole
  ( Pool
  , mapPool
  , newUnlimitedPool
  , newPool
  , withResource
  , acquireResource
  , returnResource
  , destroyResource
  , poolMetrics

  , Metrics (..)
  )
where

import qualified Control.Concurrent.Classy as Concurrent
import qualified Control.Concurrent.Classy.Async as Async
import           Control.Monad (forever, unless, void)
import qualified Control.Monad.Catch as Catch
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Foldable (for_)
import qualified Data.Sequence as Seq
import qualified Data.Time as Time
import           Numeric.Natural (Natural)

-- | Pool of resources
--
-- @since 0.0.0
data Pool m a = Pool
  { pool_acquire :: m a
  , pool_return :: a -> m ()
  , pool_destroy :: a -> m ()
  , pool_metrics :: m (Metrics Natural)
  }

-- | Lift a natural transformation @m ~> n@ to @Pool m ~> Pool n@.
--
-- @since 0.0.0
mapPool
  :: (forall x. m x -> n x)
  -> Pool m a
  -> Pool n a
mapPool to pool = Pool
  { pool_acquire = to $ pool_acquire pool
  , pool_return = to . pool_return pool
  , pool_destroy = to . pool_destroy pool
  , pool_metrics = to $ pool_metrics pool
  }

{-# INLINE mapPool #-}

-- | Pool resource
data Resource a =
  Resource
    Time.UTCTime
    -- ^ Last use time
    a
    -- ^ The resource itesemf

-- | Create a new pool that has no limit on how many resources it may create and hold.
--
-- @since 0.0.0
newUnlimitedPool
  :: (Concurrent.MonadConc m, MonadIO m)
  => m a
  -- ^ Resource creation
  -> (a -> m ())
  -- ^ Resource destruction
  -> Time.NominalDiffTime
  -- ^ Maximum idle time (+-1s) after which a resource is destroyed
  -> m (Pool m a)
newUnlimitedPool create destroy maxIdleTime = do
  leftOversRef <- Concurrent.newIORefN "leftOvers" Seq.empty

  metricRefs <- mkMetricRefs

  let
    wrappedCreate = do
      value <- create
      succIORef (metrics_createdResources metricRefs)
      pure value

    wrappedDestroy resource =
      destroy resource `Catch.finally` succIORef (metrics_destroyedResources metricRefs)

    acquireResource = do
      mbResource <- Concurrent.atomicModifyIORef' leftOversRef $ \leftOvers ->
        case leftOvers of
          Resource _ head Seq.:<| tail -> (tail, Just head)
          _empty -> (leftOvers, Nothing)
      resource <- maybe wrappedCreate pure mbResource

      numDestroyed <- Concurrent.readIORef (metrics_destroyedResources metricRefs)
      numCreated <- Concurrent.readIORef (metrics_createdResources metricRefs)
      maxIORef (metrics_maxLiveResources metricRefs) (numCreated - numDestroyed)

      pure resource

    returnResource value = do
      now <- liftIO Time.getCurrentTime
      Concurrent.atomicModifyIORef' leftOversRef $ \leftOvers ->
        (leftOvers Seq.:|> Resource now value, ())

  _reaperThread <- Async.asyncWithUnmaskN "reaperThread" $ \unmask -> unmask $ forever $ do
    now <- liftIO Time.getCurrentTime

    let isStillGood (Resource lastUse _) = Time.diffUTCTime now lastUse <= maxIdleTime
    oldResource <- Concurrent.atomicModifyIORef' leftOversRef (Seq.partition isStillGood)

    unless (null oldResource) $ void $
      Async.asyncN "destructionThread" $
        for_ oldResource $ \(Resource _ value) ->
          Catch.try @_ @Catch.SomeException $ wrappedDestroy value

    Concurrent.threadDelay 1_000_000

  pure Pool
    { pool_acquire = acquireResource
    , pool_return = returnResource
    , pool_destroy = wrappedDestroy
    , pool_metrics = readMetricRefs metricRefs
    }

-- | Similar to 'newUnlimitedPool' but allows you to limit the number of resources that will exist
-- at the same time. When all resources are currently in use, further resource acquisition will
-- block until one is no longer in use.
--
-- @since 0.0.0
newPool
  :: (Concurrent.MonadConc m, MonadIO m, MonadFail m)
  => m a
  -- ^ Resource creation
  -> (a -> m ())
  -- ^ Resource destruction
  -> Int
  -- ^ Maximum number of resources to exist at the same time
  -> Time.NominalDiffTime
  -- ^ Maximum idle time after which a resource is destroyed
  -> m (Pool m a)
newPool create destroy maxElems maxIdleTime = do
  basePool <- newUnlimitedPool create destroy maxIdleTime
  maxElemBarrier <- Concurrent.newQSem maxElems

  let
    acquireResource = Catch.mask $ \restore -> do
      Concurrent.waitQSem maxElemBarrier
      restore (pool_acquire basePool)
        `Catch.onError` Concurrent.signalQSem maxElemBarrier

    giveBackResource f value = Catch.mask $ \restore ->
      restore (f basePool value)
        `Catch.finally` Concurrent.signalQSem maxElemBarrier

  pure Pool
    { pool_acquire = acquireResource
    , pool_return = giveBackResource pool_return
    , pool_destroy = giveBackResource pool_destroy
    , pool_metrics = pool_metrics basePool
    }

-- | Use a resource from the pool. Once the continuation returns, the resource will be returned to
-- the pool. If the given continuation throws an error then the acquired resource will be destroyed
-- instead.
--
-- @since 0.0.0
withResource :: Catch.MonadMask m => Pool m a -> (a -> m r) -> m r
withResource pool f =
  Catch.mask $ \restore -> do
    resource <- restore (pool_acquire pool)
    result <- restore (f resource) `Catch.onError` pool_destroy pool resource
    pool_return pool resource
    pure result

{-# INLINE withResource #-}

-- | Acquire a resource.
--
-- @since 0.1.0
acquireResource :: Pool m a -> m a
acquireResource = pool_acquire

{-# INLINE acquireResource #-}

-- | Return a resource to the pool.
--
-- @since 0.1.0
returnResource :: Pool m a -> a -> m ()
returnResource = pool_return

{-# INLINE returnResource #-}

-- | Destroy a resource.
--
-- @since 0.1.0
destroyResource :: Pool m a -> a -> m ()
destroyResource = pool_destroy

{-# INLINE destroyResource #-}

-- | Fetch pool metrics.
--
-- @since 0.0.0
poolMetrics :: Pool m a -> m (Metrics Natural)
poolMetrics = pool_metrics

{-# INLINE poolMetrics #-}

---

-- | Pool metrics
--
-- @since 0.0.0
data Metrics a = Metrics
  { metrics_createdResources :: a
  -- ^ Total number of resources created
  --
  -- @since 0.0.0
  , metrics_destroyedResources :: a
  -- ^ Total number of resources destroyed
  --
  -- @since 0.0.0
  , metrics_maxLiveResources :: a
  -- ^ Maximum number of resources that were alive simultaneously
  --
  -- @since 0.0.0
  }
  deriving stock (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

-- | Create the IORefs which capture the metric values.
mkMetricRefs :: Concurrent.MonadConc m => m (Metrics (Concurrent.IORef m Natural))
mkMetricRefs =
  Metrics
    <$> Concurrent.newIORefN "created" 0
    <*> Concurrent.newIORefN "destroyed" 0
    <*> Concurrent.newIORefN "maxLive" 0

-- | Read all the metric values.
readMetricRefs :: Concurrent.MonadConc m => Metrics (Concurrent.IORef m a) -> m (Metrics a)
readMetricRefs = traverse Concurrent.readIORef

-- | Increase a value held by an IORef by one.
succIORef :: (Concurrent.MonadConc m, Enum a) => Concurrent.IORef m a -> m ()
succIORef ref = Concurrent.atomicModifyIORef' ref (\x -> (succ x, ()))

-- | Replace the value in an IORef with the given value if the latter is greater.
maxIORef :: (Concurrent.MonadConc m, Ord a) => Concurrent.IORef m a -> a -> m ()
maxIORef ref y = Concurrent.atomicModifyIORef' ref (\x -> (max x y, ()))
