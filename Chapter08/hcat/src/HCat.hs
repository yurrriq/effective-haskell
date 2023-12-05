{-# LANGUAGE LambdaCase #-}

module HCat where

import qualified System.Environment as Env

runHCat :: IO ()
runHCat = handleArgs >>= displayMessage
  where
    displayMessage = \case
      Left err -> putStrLn $ "Error: " <> err
      Right fname ->
        putStrLn $ "Opening file: " <> fname

handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
  where
    parseArgs = \case
      [fname] -> Right fname
      [] -> Left "no filename provided"
      _ -> Left "multiple files not supported"
