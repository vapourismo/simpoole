module Simpoole.MonadSpec (spec) where

import qualified Control.Concurrent.Classy as Conc
import qualified Control.Concurrent.Classy.Async as Async
import           Control.Monad (join)
import qualified Simpoole as Pool
import qualified Simpoole.Monad as Pool.Monad
import           Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
  describe "PoolT" $ do
    it "supports re-entrance" $ do
      counterRef <- Conc.newIORef (0 :: Integer)
      let allocate = Conc.atomicModifyIORef' counterRef (join (,) . succ)

      pool <- Pool.newPool allocate (\_ -> pure ()) Pool.defaultSettings

      (x, y) <- Pool.Monad.runPoolT pool $
        Pool.Monad.withResource $ \x ->
          Pool.Monad.withResource $ \y ->
            pure (x, y :: Integer)

      x `shouldBe` y

    it "causes new threads to get difference resources" $ do
      counterRef <- Conc.newIORef (0 :: Integer)
      let allocate = Conc.atomicModifyIORef' counterRef (join (,) . succ)

      pool <- Pool.newPool allocate (\_ -> pure ()) Pool.defaultSettings

      (x, y) <- Pool.Monad.runPoolT pool $
        Pool.Monad.withResource $ \x -> do
          ay <- Async.async $ Pool.Monad.withResource $ \y -> pure (y :: Integer)
          y <- Async.wait ay
          pure (x, y)

      x `shouldNotBe` y
