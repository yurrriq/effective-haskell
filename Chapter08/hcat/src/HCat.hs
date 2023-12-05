{-# LANGUAGE LambdaCase #-}

module HCat where

import qualified System.Environment as Env

runHCat :: IO ()
runHCat =
  handleArgs >>= \case
    Left err ->
      putStrLn $ "Error processing: " <> err
    Right fname ->
      readFile fname >>= putStrLn

handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
  where
    parseArgs = \case
      [fname] -> Right fname
      [] -> Left "no filename provided"
      _ -> Left "multiple files not supported"
