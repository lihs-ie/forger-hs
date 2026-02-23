-- |
-- Module      : Forger.Mold
-- Description : Mold definitions for generating test data
-- Copyright   : (c) 2026 lihs
-- License     : MIT
--
-- t'Mold' is the core abstraction of this library. A t'Mold' defines
-- how to generate a value of a given type from a 'Seed'.
module Forger.Mold
  ( -- * Core types
    Seed,
    Mold (..),

    -- * String generation
    MaxLength,
    MinLength,
    StringOverrides (..),
    StringMold,
    stringMold,

    -- * Enum generation
    Exclusion (..),
    EnumOverrides (..),
    EnumMold,
    enumMold,

    -- * Map generation
    MapOverrides (..),
    MapMold,
    mapMold,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Forger.Internal (alphanumeric, scramble)

-- | Seed value for deterministic generation.
type Seed = Int

-- | A mold defines how to generate a value from a 'Seed'.
-- See 'pour' for the core generation function.
--
-- @value@ is the type of the generated value.
-- @overrides@ is the type used to customize generation.
newtype Mold value overrides = Mold
  { pour :: Maybe overrides -> Seed -> value
    -- ^ Generate a value from optional overrides and a seed.
  }

-- | Overrides for 'stringMold'.
newtype StringOverrides = StringOverrides
  { stringValue :: Maybe String
    -- ^ If 'Just', use this value directly instead of generating one.
  }

-- | Maximum length of a generated string.
type MaxLength = Int

-- | Minimum length of a generated string.
type MinLength = Int

-- | A t'Mold' specialized for 'String' generation.
type StringMold = Mold String StringOverrides

-- | Create a t'Mold' that generates strings.
--
-- The generated string length is determined by the seed within
-- the range of @minimumLength@ to @maximumLength@.
stringMold
  :: Maybe MaxLength
  -- ^ Maximum length (default: 255).
  -> Maybe MinLength
  -- ^ Minimum length (default: 1).
  -> Maybe [Char]
  -- ^ Character set to use (default: 'alphanumeric').
  -> StringMold
stringMold maximumLength minimumLength characters =
  let minLength = fromMaybe 1 minimumLength
      maxLength = fromMaybe 255 maximumLength
      candidates = fromMaybe alphanumeric characters
   in Mold $ \maybeOverrides seed ->
        let offset = seed `mod` (maxLength - minLength + 1)
            generatedLength = minLength + offset
            generatedValue =
              map
                ( \index ->
                    let characterIndex = scramble (seed + index) `mod` length candidates
                     in candidates !! characterIndex
                )
                [0 .. generatedLength - 1]
         in fromMaybe generatedValue (maybeOverrides >>= stringValue)

-- | Exclusion rule for filtering candidates in 'enumMold'.
data Exclusion value
  = -- | No values are excluded.
    NoExclusion
  | -- | Exclude a single value.
    ExcludeSingle value
  | -- | Exclude multiple values.
    ExcludeMultiple (Set.Set value)

-- | Overrides for 'enumMold'.
data EnumOverrides value = EnumOverrides
  { overrideEnumValue :: Maybe value
    -- ^ If 'Just', use this value directly instead of selecting from candidates.
  , overrideExclusion :: Exclusion value
    -- ^ Exclusion rule applied before selecting a candidate.
  }

isExcluded :: (Ord value) => Exclusion value -> value -> Bool
isExcluded NoExclusion _ = False
isExcluded (ExcludeSingle excluded) candidate = excluded == candidate
isExcluded (ExcludeMultiple excludedSet) candidate = Set.member candidate excludedSet

-- | A t'Mold' specialized for selecting from enum-like candidates.
type EnumMold value = Mold value (EnumOverrides value)

-- | Create a t'Mold' that selects a value from a list of candidates.
--
-- Throws an error if all candidates are excluded via 'overrideExclusion'.
enumMold :: (Ord value) => [value] -> EnumMold value
enumMold candidates =
  Mold $ \maybeOverrides seed ->
    let exclusion = maybe NoExclusion overrideExclusion maybeOverrides
        availableCandidates = filter (not . isExcluded exclusion) candidates
        selectedValue =
          if null availableCandidates
            then error "Candidates does not exist."
            else availableCandidates !! (seed `mod` length availableCandidates)
     in fromMaybe selectedValue (maybeOverrides >>= overrideEnumValue)

-- | Overrides for 'mapMold'.
newtype MapOverrides key value = MapOverrides
  { overridesEntries :: Maybe [(key, value)]
    -- ^ If 'Just', use these entries directly instead of generating them.
  }

-- | A t'Mold' specialized for 'Data.Map.Strict.Map' generation.
type MapMold key value = Mold (Map.Map key value) (MapOverrides key value)

-- | Create a t'Mold' that generates a 'Data.Map.Strict.Map'.
--
-- Keys and values are generated using the provided molds.
-- The number of entries is determined by the seed (1 to 10).
mapMold ::
  (Ord key) =>
  Mold key keyOverrides ->
  -- ^ Mold for generating keys.
  Mold value valueOverrides ->
  -- ^ Mold for generating values.
  MapMold key value
mapMold keyMold valueMold =
  Mold $ \maybeOverrides seed ->
    let entryCount = (seed `mod` 10) + 1
        generatedEntries =
          map
            ( \index ->
                let keyInstance = pour keyMold Nothing (seed + index)
                    valueInstance = pour valueMold Nothing (seed + index)
                 in (keyInstance, valueInstance)
            )
            [0 .. entryCount - 1]
     in Map.fromList (fromMaybe generatedEntries (maybeOverrides >>= overridesEntries))
