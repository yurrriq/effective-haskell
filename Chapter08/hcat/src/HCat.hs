{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import qualified Control.Exception as Exception
import qualified System.Environment as Env
import qualified System.IO.Error as IOError

runHCat :: IO ()
runHCat =
  withErrorHandling $
    handleArgs
      >>= eitherToError
      >>= readFile
      >>= putStrLn
  where
    withErrorHandling =
      Exception.handle $ \err ->
        putStrLn "I ran into an error:" >> print @IOError err

handleArgs :: IO (Either String FilePath)
handleArgs = parseArgs <$> Env.getArgs
  where
    parseArgs = \case
      [fname] -> Right fname
      [] -> Left "no filename provided"
      _ -> Left "multiple files not supported"

eitherToError :: (Show a) => Either a b -> IO b
eitherToError = either (Exception.throwIO . IOError.userError . show) return
