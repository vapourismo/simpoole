{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Simpoole.Internal (FailToIO (..), failToIO) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Coerce (coerce)

newtype FailToIO m a = FailToIO (m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadConc
    )

instance MonadIO m => MonadFail (FailToIO m) where
  fail = liftIO . fail

  {-# INLINE fail #-}

failToIO :: FailToIO m a -> m a
failToIO = coerce

{-# INLINE failToIO #-}
