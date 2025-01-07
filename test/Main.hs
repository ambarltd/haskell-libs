module Main where

import Test.Hspec (hspec, parallel)

import Test.OnDemand (testOnDemand)
import Test.Warden (testWarden)

{- | Note [How tests work]

# Running a subset of tests

Example: match on test description

  ./util.sh test -- --match "typecheck"

 -}
main :: IO ()
main =
  hspec $ parallel $ do
    -- unit tests use the projector library
    testOnDemand
    testWarden
