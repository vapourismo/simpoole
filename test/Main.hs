module Main (main) where

import qualified Simpoole.MonadSpec
import qualified SimpooleSpec
import           Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Simpoole" SimpooleSpec.spec
  describe "Simpoole.Monad" Simpoole.MonadSpec.spec
