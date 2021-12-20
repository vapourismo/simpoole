module Main (main) where

import qualified SimpooleSpec
import           Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $
  describe "Simpoole" SimpooleSpec.spec
