{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad
import System.Console.Haskeline
import System.Directory

import Hal.Printer
import Hal.Reader
import Hal.Types

halEval :: HalValue -> HalValue
halEval = id

rep :: String -> [String]
rep (halRead -> Right val) = map (halPrint . halEval) $ val
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
