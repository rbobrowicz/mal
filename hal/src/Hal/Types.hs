
module Hal.Types where

import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq)
import GHC.Exts (IsList(..))

type HalEnv = HashMap String HalValue
data HalFunc = BuiltIn String ([HalValue] -> HalValue)

instance Eq HalFunc where
  (==) (BuiltIn x _) (BuiltIn y _) = x == y

data HalValue = HalList [HalValue]
              | HalSymbol String
              | HalInt Int
              | HalString String
              | HalTrue
              | HalFalse
              | HalNil
              | HalKeyword String
              | HalVector (Seq HalValue)
              | HalHashMap (HashMap HalValue HalValue)
              | HalFunc HalFunc
  deriving (Eq)

-- Unfortunatel there is no premade instance of Hashable (Seq a)
-- So we have to roll our own instance of Hashable HalValue
instance Hashable HalValue where
  hashWithSalt s (HalList xs) = s `hashWithSalt` (0 :: Int) `hashWithSalt` xs
  hashWithSalt s (HalSymbol sym) = s `hashWithSalt` (1 :: Int) `hashWithSalt` sym
  hashWithSalt s (HalInt i) = s `hashWithSalt` (2 :: Int) `hashWithSalt` i
  hashWithSalt s (HalString str) = s `hashWithSalt` (3 :: Int) `hashWithSalt` str
  hashWithSalt s HalTrue = s `hashWithSalt` (4 :: Int)
  hashWithSalt s HalFalse = s `hashWithSalt` (5 :: Int)
  hashWithSalt s HalNil = s `hashWithSalt` (6 :: Int)
  hashWithSalt s (HalKeyword kw) = s `hashWithSalt` (7 :: Int) `hashWithSalt` kw
  hashWithSalt s (HalVector v) = s `hashWithSalt` (8 :: Int) `hashWithSalt` toList v
  hashWithSalt s (HalHashMap m) = s `hashWithSalt` (9 :: Int) `hashWithSalt` m
  hashWithSalt s (HalFunc (BuiltIn x _)) = s `hashWithSalt` (10 :: Int) `hashWithSalt` x

