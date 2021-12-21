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

  , Settings (..)
  , defaultSettings

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

-- | Strategy to use when returning resources to the pool
--
-- @since 0.1.0
data ReturnPolicy
  = ReturnToFront
  -- ^ Return resources to the front. Resources that have been used recently are more likely to be
  -- reused again quicker. This strategy is good if you want to scale down the pool more quickly in
  -- case resources are not needed.
  --
  -- @since 0.1.0
  | ReturnToBack
  -- ^ Return resources to the back. Resources that have been used recently are less likely to be
  -- used again quicker. Use this strategy if you want to keep more resources in the pool fresh, or
  -- when maintaining the pool in order to be ready for burst workloads.
  -- This strategy can lead to no resources ever been freed when all resources are used within the
  -- idle timeout.
  --
  -- @since 0.1.0
  | ReturnToMiddle
  -- ^ Return resources to the middle. This offers a middleground between 'ReturnToFront' and
  -- 'ReturnToBack'. By ensuring that the starting sub-sequence of resources is reused quicker but
  -- the trailing sub-sequence is not and therefore released more easily.
  --
  -- @since 0.1.0
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

-- | Insert a value based on the return policy.
applyReturnPolicy :: ReturnPolicy -> a -> Seq.Seq a -> Seq.Seq a
applyReturnPolicy policy value seq =
  case policy of
    ReturnToFront -> value Seq.<| seq
    ReturnToBack -> seq Seq.|> value
    ReturnToMiddle -> Seq.insertAt middleIndex value seq
  where
    middleIndex
      | even (Seq.length seq) = div (Seq.length seq) 2
      | otherwise = div (Seq.length seq) 2 + 1

-- | Lets you configure certain behaviours of the pool
--
-- @since 0.1.0
data Settings = PoolSettings
  { settings_idleTimeout :: Time.NominalDiffTime
  -- ^ Maximum idle time after which a resource is destroyed
  --
  -- @since 0.1.0
  , settings_returnPolicy :: ReturnPolicy
  }

-- | Default pool settings
--
-- @since 0.1.0
defaultSettings :: Settings
defaultSettings = PoolSettings
  { settings_idleTimeout = 60 -- 60 seconds
  , settings_returnPolicy = ReturnToMiddle
  }

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
-- @since 0.1.0
newUnlimitedPool
  :: (Concurrent.MonadConc m, MonadIO m)
  => m a
  -- ^ Resource creation
  -> (a -> m ())
  -- ^ Resource destruction
  -> Settings
  -- ^ Pool settings
  -> m (Pool m a)
newUnlimitedPool create destroy settings = do
  leftOversRef <- Concurrent.newIORefN "leftOvers" Seq.empty

  createdRef <- Concurrent.newIORefN "created" 0
  destroyedRef <- Concurrent.newIORefN "destroyed" 0
  maxLiveRef <- Concurrent.newIORefN "maxLive" 0

  let
    getMetrics = do
      created <- Concurrent.readIORef createdRef
      destroyed <- Concurrent.readIORef destroyedRef
      maxLive <- Concurrent.readIORef maxLiveRef
      leftOvers <- Concurrent.readIORef leftOversRef

      pure Metrics
        { metrics_createdResources = created
        , metrics_destroyedResources = destroyed
        , metrics_maxLiveResources = maxLive
        , metrics_idleResources = fromIntegral (Seq.length leftOvers)
        }

    wrappedCreate = do
      value <- create
      succIORef createdRef
      pure value

    wrappedDestroy resource =
      destroy resource `Catch.finally` succIORef destroyedRef

    acquireResource = do
      mbResource <- Concurrent.atomicModifyIORef' leftOversRef $ \leftOvers ->
        case leftOvers of
          Resource _ head Seq.:<| tail -> (tail, Just head)
          _empty -> (leftOvers, Nothing)

      resource <- maybe wrappedCreate pure mbResource

      numDestroyed <- Concurrent.readIORef destroyedRef
      numCreated <- Concurrent.readIORef createdRef
      maxIORef maxLiveRef (numCreated - numDestroyed)

      pure resource

    returnResource value = do
      now <- liftIO Time.getCurrentTime
      Concurrent.atomicModifyIORef' leftOversRef $ \leftOvers ->
        ( applyReturnPolicy (settings_returnPolicy settings) (Resource now value) leftOvers
        , ()
        )

  _reaperThread <- Async.asyncWithUnmaskN "reaperThread" $ \unmask -> unmask $ forever $ do
    now <- liftIO Time.getCurrentTime

    let
      isStillGood (Resource lastUse _) =
        Time.diffUTCTime now lastUse <= settings_idleTimeout settings

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
    , pool_metrics = getMetrics
    }

-- | Similar to 'newUnlimitedPool' but allows you to limit the number of resources that will exist
-- at the same time. When all resources are currently in use, further resource acquisition will
-- block until one is no longer in use.
--
-- @since 0.1.0
newPool
  :: (Concurrent.MonadConc m, MonadIO m, MonadFail m)
  => m a
  -- ^ Resource creation
  -> (a -> m ())
  -- ^ Resource destruction
  -> Int
  -- ^ Maximum number of resources to exist at the same time
  -> Settings
  -- ^ Pool settings
  -> m (Pool m a)
newPool create destroy maxElems settings = do
  basePool <- newUnlimitedPool create destroy settings
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
  , metrics_idleResources :: a
  -- ^ Number of resources currently idle
  --
  -- @since 0.1.0
  }
  deriving stock (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

-- | Increase a value held by an IORef by one.
succIORef :: (Concurrent.MonadConc m, Enum a) => Concurrent.IORef m a -> m ()
succIORef ref = Concurrent.atomicModifyIORef' ref (\x -> (succ x, ()))

-- | Replace the value in an IORef with the given value if the latter is greater.
maxIORef :: (Concurrent.MonadConc m, Ord a) => Concurrent.IORef m a -> a -> m ()
maxIORef ref y = Concurrent.atomicModifyIORef' ref (\x -> (max x y, ()))
