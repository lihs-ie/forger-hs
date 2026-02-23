# forger

A Haskell library for generating test data with deterministic, seed-based randomness.

Haskell port of [forger-ts](https://www.npmjs.com/package/@lihs-ie/forger-ts).

## Features

- **Deterministic generation** -- Same seed always produces the same output, making tests reproducible
- **Composable Molds** -- Define how to generate each type and compose them together
- **Optional overrides** -- Customize specific fields while generating the rest automatically
- **Built-in Molds** -- `StringMold`, `EnumMold`, `MapMold` included out of the box

## Installation

Add `forger` to your `build-depends` in your `.cabal` file:

```cabal
build-depends:
    forger
```

## Quick start

```haskell
import Forger

-- Generate a random string
main :: IO ()
main = do
    value <- forge (stringMold Nothing Nothing Nothing) Nothing
    print value
```

## Usage

### Defining a custom Mold

A `Mold` defines how to generate a value of a given type. Create one by providing a function that takes optional overrides and a seed:

```haskell
import Data.Maybe (fromMaybe)
import Forger

data User = User
  { userName :: String
  , userAge  :: Int
  } deriving stock (Show)

data UserOverrides = UserOverrides
  { overrideName :: Maybe String
  , overrideAge  :: Maybe Int
  }

userMold :: Mold User UserOverrides
userMold = Mold $ \maybeOverrides seed ->
  User
    { userName = fromMaybe
        (forgeWithSeed (stringMold (Just 20) (Just 5) Nothing) Nothing seed)
        (maybeOverrides >>= overrideName)
    , userAge = fromMaybe
        (seed `mod` 100)
        (maybeOverrides >>= overrideAge)
    }
```

### Generating values

```haskell
-- Random seed (IO)
user <- forge userMold Nothing

-- Fixed seed (pure, deterministic)
let user = forgeWithSeed userMold Nothing 42

-- With overrides
let user = forgeWithSeed userMold (Just (UserOverrides (Just "Alice") Nothing)) 42
-- user == User { userName = "Alice", userAge = 42 }

-- Multiple values with random seeds
users <- forgeMulti userMold 10 Nothing

-- Multiple values with sequential seeds (deterministic)
let users = forgeMultiWithSeed userMold 10 Nothing 0
```

### StringMold

Generates strings from a configurable character set and length range:

```haskell
-- Default: 1-255 characters from alphanumeric set
let mold = stringMold Nothing Nothing Nothing

-- Custom length range
let mold = stringMold (Just 20) (Just 5) Nothing

-- Custom character set
let mold = stringMold (Just 10) (Just 10) (Just numeric)

-- Override with a specific value
let value = forgeWithSeed mold (Just (StringOverrides (Just "custom"))) 42
-- value == "custom"
```

Available character sets: `alphanumeric`, `alpha`, `numeric`, `slug`, `symbolCharacters`

### EnumMold

Selects from a list of candidates with optional exclusion:

```haskell
data Color = Red | Green | Blue
  deriving stock (Bounded, Enum, Ord, Eq, Show)

colorMold :: EnumMold Color
colorMold = enumMold [minBound .. maxBound]

-- Generate a random color
let color = forgeWithSeed colorMold Nothing 42

-- Override with a specific value
let color = forgeWithSeed colorMold (Just (EnumOverrides (Just Red) NoExclusion)) 0
-- color == Red

-- Exclude values
let overrides = EnumOverrides Nothing (ExcludeSingle Red)
let color = forgeWithSeed colorMold (Just overrides) 42
-- color is never Red
```

### MapMold

Generates `Map` values by composing key and value molds:

```haskell
import Data.Map.Strict qualified as Map

let keyMold   = stringMold (Just 10) (Just 5) Nothing
    valueMold = stringMold (Just 20) (Just 10) Nothing
    mold      = mapMold keyMold valueMold

-- Generate a map with 1-10 entries
let result = forgeWithSeed mold Nothing 42

-- Override with specific entries
let overrides = MapOverrides (Just [("key1", "value1"), ("key2", "value2")])
let result = forgeWithSeed mold (Just overrides) 42
```

## Testing with forger

### Setting up test fixtures

Define your domain types and their molds in a shared fixture module:

```haskell
-- test/Support/Fixture.hs
module Support.Fixture
  ( User (..), UserOverrides (..), userMold
  ) where

import Data.Maybe (fromMaybe)
import Forger

data User = User
  { userName :: String
  , userAge  :: Int
  } deriving stock (Eq, Show)

data UserOverrides = UserOverrides
  { overrideName :: Maybe String
  , overrideAge  :: Maybe Int
  }

userMold :: Mold User UserOverrides
userMold = Mold $ \maybeOverrides seed ->
  User
    { userName = fromMaybe
        (forgeWithSeed (stringMold (Just 20) (Just 5) Nothing) Nothing seed)
        (maybeOverrides >>= overrideName)
    , userAge = fromMaybe
        (seed `mod` 100)
        (maybeOverrides >>= overrideAge)
    }
```

### Writing tests with Hspec

```haskell
-- test/UserSpec.hs
module UserSpec (spec) where

import Forger
import Support.Fixture
import Test.Hspec

spec :: Spec
spec = do
  describe "User" $ do
    it "should generate a user with deterministic values" $ do
      let user = forgeWithSeed userMold Nothing 42
      userName user `shouldSatisfy` (not . null)
      userAge user `shouldBe` 42

    it "should allow overriding specific fields" $ do
      let overrides = UserOverrides (Just "Alice") Nothing
          user = forgeWithSeed userMold (Just overrides) 42
      userName user `shouldBe` "Alice"
      userAge user `shouldBe` 42

    it "should produce consistent results for the same seed" $ do
      let user1 = forgeWithSeed userMold Nothing 100
          user2 = forgeWithSeed userMold Nothing 100
      user1 `shouldBe` user2

    it "should generate multiple unique users" $ do
      users <- forgeMulti userMold 5 Nothing
      length users `shouldBe` 5
```

### Key patterns

**Generate without caring about specific values** -- use `Nothing` for overrides and let the seed handle it:

```haskell
let user = forgeWithSeed userMold Nothing 42
```

**Pin specific fields for assertions** -- override only the fields you care about in the test:

```haskell
let user = forgeWithSeed userMold (Just (UserOverrides (Just "Bob") Nothing)) 42
userName user `shouldBe` "Bob"
-- userAge is generated automatically from the seed
```

**Generate many instances for property-like checks**:

```haskell
users <- forgeMulti userMold 100 Nothing
all (\u -> userAge u >= 0 && userAge u < 100) users `shouldBe` True
```

## API reference

### Generation functions

| Function | Description |
|---|---|
| `forge` | Generate a value with a random seed (`IO`) |
| `forgeWithSeed` | Generate a value with a specific seed (pure) |
| `forgeMulti` | Generate multiple values with random seeds (`IO`) |
| `forgeMultiWithSeed` | Generate multiple values with sequential seeds (pure) |

### Mold types

| Type | Description |
|---|---|
| `Mold value overrides` | Core type defining how to generate a value |
| `StringMold` | `Mold String StringOverrides` |
| `EnumMold value` | `Mold value (EnumOverrides value)` |
| `MapMold key value` | `Mold (Map key value) (MapOverrides key value)` |

### Mold constructors

| Function | Description |
|---|---|
| `stringMold` | Create a string mold with configurable length and character set |
| `enumMold` | Create a mold that selects from a list of candidates |
| `mapMold` | Create a map mold by composing key and value molds |

Full API documentation is available via Haddock:

```bash
cabal haddock
```

## GHC compatibility

Tested with:

- GHC 9.6.7
- GHC 9.8.4
- GHC 9.10.3

## License

MIT
