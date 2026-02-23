{- |
Module      : Forger
Description : A library for generating test data
Copyright   : (c) 2026 lihs
License     : MIT
Maintainer  : lihs-ie

Forger provides a composable, seed-based approach to generating
test data in Haskell. This is the main entry point that
re-exports the public API.

== Basic usage

@
import Forger

-- Generate a random string
value <- forge (stringMold Nothing Nothing Nothing) Nothing

-- Generate a deterministic string with a fixed seed
let value = forgeWithSeed (stringMold Nothing Nothing Nothing) Nothing 42
@
-}
module Forger (
    -- * Generation
    forge,
    forgeWithSeed,
    Size,
    forgeMulti,
    forgeMultiWithSeed,

    -- * Molds and overrides
    module Forger.Mold,

    -- * Character sets
    alphanumeric,
    alpha,
    slug,
    numeric,
    symbolCharacters,
)
where

import Control.Monad (replicateM)
import Forger.Internal
import Forger.Mold
import System.Random (randomRIO)

-- | Generate a value with a random seed.
forge :: Mold value overrides -> Maybe overrides -> IO value
forge mold overrides = do
    seed <- randomRIO (0, maxBound)
    pure $ pour mold overrides seed

{- | Generate a value with a specific seed.
Same seed always produces the same result.
-}
forgeWithSeed :: Mold value overrides -> Maybe overrides -> Seed -> value
forgeWithSeed = pour

-- | Number of values to generate.
type Size = Int

-- | Generate multiple values, each with a random seed.
forgeMulti :: Mold value overrides -> Size -> Maybe overrides -> IO [value]
forgeMulti mold size overrides = do
    seeds <- replicateM size (randomRIO (0, maxBound))
    pure $ map (pour mold overrides) seeds

-- | Generate multiple values from sequential seeds starting at the given seed.
forgeMultiWithSeed :: Mold value overrides -> Size -> Maybe overrides -> Seed -> [value]
forgeMultiWithSeed mold size overrides seed = map (\index -> pour mold overrides (seed + index)) [0 .. size - 1]
