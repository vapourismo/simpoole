{-# LANGUAGE NumericUnderscores #-}

module SimpooleSpec (spec) where

import qualified Control.Concurrent.Classy as Concurrent
import qualified Control.Concurrent.Classy.Async as Async
import           Control.Monad (join)
import           Numeric.Natural (Natural)
import qualified Simpoole as Pool
import           Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "newPool" $ do
    it "eventually frees all resources" $ do
      counterRef <- Concurrent.newIORefN "counterRef" (0 :: Integer)

      let
        create =
          Concurrent.atomicModifyIORef' counterRef $ \count -> (succ count, ())

        destroy _ =
          Concurrent.atomicModifyIORef' counterRef $ \count -> (pred count, ())

      pool <- Pool.newPool create destroy
        Pool.defaultSettings { Pool.settings_idleTimeout = Just 1 }

      Async.replicateConcurrently_ 200 $
        Pool.withResource pool $ const $ Concurrent.threadDelay 1_000

      currentCount <- Concurrent.readIORef counterRef
      currentCount `shouldSatisfy` (> 0)

      let
        wait = do
          currentCount <- Concurrent.readIORef counterRef
          if currentCount > 0 then do
            Concurrent.threadDelay 100_000
            wait
          else
            pure ()

      wait

      currentCount <- Concurrent.readIORef counterRef
      currentCount `shouldBe` 0

    it "tracks metrics accurately" $ do
      counterRef <- Concurrent.newIORefN "counterRef" (0 :: Integer)
      createdRef <- Concurrent.newIORefN "createdRef" (0 :: Natural)
      destroyedRef <- Concurrent.newIORefN "destroyedRef" (0 :: Natural)
      maxRef <- Concurrent.newIORefN "maxRef" 0

      let
        create = do
          Concurrent.atomicModifyIORef' createdRef $ \count -> (count + 1, ())
          counter <- Concurrent.atomicModifyIORef' counterRef $ join (,) . succ
          Concurrent.atomicModifyIORef' maxRef $ join (,) . max counter

        destroy _ = do
          Concurrent.atomicModifyIORef' destroyedRef $ \count -> (count + 1, ())
          Concurrent.atomicModifyIORef' counterRef $ \count -> (count - 1, ())

      pool <- Pool.newPool create destroy Pool.defaultSettings

      Async.replicateConcurrently_ 200 $ do
        Pool.withResource pool $ const $ Concurrent.threadDelay 1_000

      metrics <- Pool.poolMetrics pool

      -- After finishing the workload, all resources should be idle.
      Pool.metrics_maxLiveResources metrics `shouldBe` Pool.metrics_idleResources metrics

      created <- Concurrent.readIORef createdRef
      Pool.metrics_createdResources metrics `shouldBe` created
      Pool.metrics_createdResources metrics `shouldBe` Pool.metrics_maxLiveResources metrics

      destroyed <- Concurrent.readIORef destroyedRef
      Pool.metrics_destroyedResources metrics `shouldBe` destroyed

      maxLive <- Concurrent.readIORef maxRef
      fromIntegral (Pool.metrics_maxLiveResources metrics) `shouldBe` maxLive

    it "never allocates more than allowed" $ do
      counterRef <- Concurrent.newIORefN "counterRef" (0 :: Integer)
      maxRef <- Concurrent.newIORefN "maxRef" 0

      let
        create = do
          counter <- Concurrent.atomicModifyIORef' counterRef $ join (,) . succ
          Concurrent.atomicModifyIORef' maxRef $ join (,) . max counter

        destroy _ =
          Concurrent.atomicModifyIORef' counterRef $ \count -> (pred count, ())

      pool <- Pool.newPool create destroy Pool.defaultSettings
        { Pool.settings_maxLiveLimit = Just 10 }

      Async.replicateConcurrently_ 200 $ do
        Pool.withResource pool $ const $ Concurrent.threadDelay 1_000

      max <- Concurrent.readIORef maxRef
      max `shouldSatisfy` (<= 10)


  pure ()
