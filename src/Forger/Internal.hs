-- |
-- Module      : Forger.Internal
-- Description : Internal utilities for deterministic generation
-- Copyright   : (c) 2026 lihs
-- License     : MIT
--
-- Provides a deterministic hash function ('scramble') and
-- predefined character sets used by 'Forger.Mold.stringMold'.
module Forger.Internal
  ( -- * Hash function
    scramble,

    -- * Character sets
    alphanumeric,
    alpha,
    slug,
    numeric,
    symbolCharacters,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word32)

bitReverse :: Word32 -> Word32
bitReverse original = foldl' step original (zip paddings masks)
  where
    paddings :: [Int]
    paddings = [1, 2, 4, 8, 16]
    masks :: [Word32]
    masks = [0x55555555, 0x33333333, 0x0F0F0F0F, 0x00FF00FF, 0xFFFFFFFF]
    step :: Word32 -> (Int, Word32) -> Word32
    step carry (padding, mask) =
      let left = (carry `shiftR` padding) .&. mask
          right = (carry .&. mask) `shiftL` padding
       in left .|. right

extendedGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD 0 b = (b, 0, 1)
extendedGCD a b =
  let (greatestCommonDivisor, y, x) = extendedGCD (b `mod` a) a
   in (greatestCommonDivisor, x - (b `div` a) * y, y)

modularInverse :: Integer -> Integer -> Word32
modularInverse a modulus =
  let (greatestCommonDivisor, x, _) = extendedGCD a modulus
   in if greatestCommonDivisor /= 1
        then error $ "No inverse is found for " ++ show a
        else fromIntegral (x `mod` modulus)

salt :: Word32
salt = 0x17654321

invertedSalt :: Word32
invertedSalt = modularInverse (fromIntegral salt) (fromIntegral (maxBound :: Word32) + 1)

-- | Deterministic hash function. Same input always returns the same output.
scramble :: Int -> Int
scramble original =
  let normalized = fromIntegral original :: Word32
      base = normalized * salt
      inverted = bitReverse base
      result = inverted * invertedSalt
   in fromIntegral result

-- | Alphanumeric characters: @a-z@, @A-Z@, @0-9@ (62 characters).
alphanumeric :: [Char]
alphanumeric = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

-- | Alphabetic characters: @a-z@, @A-Z@ (52 characters).
alpha :: [Char]
alpha = ['a' .. 'z'] ++ ['A' .. 'Z']

-- | URL-safe slug characters: @a-z@, @0-9@, @-@ (37 characters).
slug :: [Char]
slug = ['a' .. 'z'] ++ ['0' .. '9'] ++ ['-']

-- | Numeric characters: @0-9@ (10 characters).
numeric :: [Char]
numeric = ['0' .. '9']

-- | Symbol characters: @!-/@, @:-\@@, @[-`@, @{-~@ (32 characters).
symbolCharacters :: [Char]
symbolCharacters = ['!' .. '/'] ++ [':' .. '@'] ++ ['[' .. '`'] ++ ['{' .. '~']
