module Support.Fixture (
    Person (..),
    PersonOverrides (..),
    personMold,
    Animal (..),
    animalMold,
)
where

import Data.Maybe (fromMaybe)
import Forger (EnumMold, Mold (Mold), enumMold, forgeWithSeed, stringMold)

data Person = Person
    { name :: String
    , age :: Int
    }
    deriving stock (Eq, Show)

data PersonOverrides = PersonOverrides
    { overrideName :: Maybe String
    , overrideAge :: Maybe Int
    }

personMold :: Mold Person PersonOverrides
personMold = Mold $ \maybeOverrides seed ->
    Person
        { name =
            fromMaybe
                (forgeWithSeed (stringMold Nothing Nothing Nothing) Nothing seed)
                (maybeOverrides >>= overrideName)
        , age = fromMaybe (seed `mod` 100) (maybeOverrides >>= overrideAge)
        }

data Animal = Dog | Cat | Bird deriving stock (Bounded, Enum, Ord, Eq, Show)

animalMold :: EnumMold Animal
animalMold = enumMold ([minBound .. maxBound] :: [Animal])
