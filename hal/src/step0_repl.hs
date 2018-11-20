{-# LANGUAGE LambdaCase #-}

module Main where

import System.Console.Haskeline
import System.Directory

halRead :: String -> String
halRead = id

halEval :: String -> String
halEval = id

halPrint :: String -> String
halPrint = id

rep :: String -> String
rep = halPrint . halEval . halRead


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
          outputStrLn . rep $ input
          loop
