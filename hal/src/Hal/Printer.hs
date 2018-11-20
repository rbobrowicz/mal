
module Hal.Printer
(
  halPrint
) where

import GHC.Exts (IsList(..))

import qualified Data.HashMap.Strict as HM

import Hal.Types

halPrint :: HalValue -> String
halPrint (HalList xs) = "(" ++ unwords (map halPrint xs) ++ ")"
halPrint (HalInt i) = show i
halPrint (HalSymbol s) = s
halPrint (HalString s) = show s
halPrint (HalKeyword s) = ':' : s
halPrint (HalVector xs) = "[" ++ unwords (map halPrint . toList $ xs) ++ "]"
halPrint (HalHashMap xs) = "{" ++ unwords (map printKV . HM.toList $ xs) ++ "}"
halPrint HalTrue = "true"
halPrint HalFalse = "false"
halPrint HalNil = "nil"

printKV :: (HalValue, HalValue) -> String
printKV (k,v) = halPrint k ++ " " ++ halPrint v
