{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Data.Bifunctor
import System.Console.Haskeline
import System.Directory

import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as S

import Hal.Printer
import Hal.Reader
import Hal.Types

halAdd :: HalValue
halAdd = HalFunc $ BuiltIn "+" (go 0)
  where
    go !a [] = HalInt a
    go !a ((HalInt y):ys) = go (a + y) ys
    go _ _ = error "Arguments to + must be integers"

halSub :: HalValue
halSub = HalFunc $ BuiltIn "-" go
  where
    go [] = error "- requires at least one argument"
    go [HalInt x] = HalInt . negate $ x
    go [HalInt x, HalInt y] = HalInt $ x - y
    go ((HalInt x):(HalInt y):zs) = go $ (HalInt $ x - y) : zs
    go _ = error "Argument to - must be integers"

halMul :: HalValue
halMul = HalFunc $ BuiltIn "*" (go 1)
  where
    go !a [] = HalInt a
    go !a ((HalInt y):ys) = go (a * y) ys
    go _ _ = error "Argument to * must be integers"

halDiv :: HalValue
halDiv = HalFunc $ BuiltIn "-" go
  where
    go [] = error "/ requires at least two arguments"
    go [_] = error "/ requires at least two arguments"
    go [HalInt x, HalInt y] = HalInt $ x `div` y
    go ((HalInt x):(HalInt y):zs) = go $ (HalInt $ x `div` y) : zs
    go _ = error "Argument to - must be integers"

env :: HalEnv
env = HM.fromList [("+", halAdd), ("-", halSub), ("*", halMul), ("/", halDiv)]

halEval :: HalEnv -> HalValue -> HalValue
halEval e (HalList xs) = case map (halEval e) xs of
  [] -> HalList []
  (HalFunc (BuiltIn _ f):args) -> f args
  (v:_) -> error $ halPrint v ++ " is not a function"
halEval e (HalSymbol s) = case HM.lookup s e of
  Nothing -> error $ "'" ++ s ++ "' not found"
  Just v -> v
halEval e (HalVector xs) = HalVector . S.mapWithIndex (const $ halEval e) $ xs
halEval e (HalHashMap m) = HalHashMap . HM.fromList . map (bimap (halEval e) (halEval e)) . HM.toList $ m
halEval _ v = v


rep :: String -> [String]
rep (halRead -> Right vals) = map (halPrint . halEval env) $ vals
rep (halRead -> Left err) = [err]
rep _ = error "This shouldn't happen"


haskelineSettings :: FilePath -> Settings IO
haskelineSettings fp = defaultSettings
  { historyFile = Just fp
  }

main :: IO ()
main = do
  dataDir <- getXdgDirectory XdgData "mal"
  createDirectoryIfMissing True dataDir
  let histFile = dataDir ++ "/history"
  runInputT (haskelineSettings histFile) loop
    where
      loop :: InputT IO ()
      loop = getInputLine "user> " >>= \case
        Nothing -> pure ()
        Just input -> do
          forM_ (rep input) outputStrLn
          loop
