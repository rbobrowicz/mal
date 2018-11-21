
module Hal.Printer
(
  halPrint
) where

import GHC.Exts (IsList(..))

import qualified Data.HashMap.Strict as HM

import Hal.Types

halPrint :: [HalValue] -> [String]
halPrint = map printVal

printVal :: HalValue -> String
printVal (HalList xs) = "(" ++ unwords (map printVal xs) ++ ")"
printVal (HalInt i) = show i
printVal (HalSymbol s) = s
printVal (HalString s) = show s
printVal (HalKeyword s) = ':' : s
printVal (HalVector xs) = "[" ++ unwords (map printVal . toList $ xs) ++ "]"
printVal (HalHashMap xs) = "{" ++ unwords (map printKV . HM.toList $ xs) ++ "}"
printVal HalTrue = "true"
printVal HalFalse = "false"
printVal HalNil = "nil"

printKV :: (HalValue, HalValue) -> String
printKV (k,v) = printVal k ++ " " ++ printVal v
