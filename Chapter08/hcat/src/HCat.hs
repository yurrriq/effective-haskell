{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import qualified Control.Exception as Exception
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified System.Environment as Env
import qualified System.IO.Error as IOError

runHCat :: IO ()
runHCat =
  withErrorHandling $
    handleArgs
      >>= eitherToError
      >>= TextIO.readFile
      >>= TextIO.putStrLn
  where
    withErrorHandling =
      Exception.handle $ \err ->
        TextIO.putStrLn "I ran into an error:" >> print @IOError err

handleArgs :: IO (Either Text FilePath)
handleArgs = parseArgs <$> Env.getArgs
  where
    parseArgs = \case
      [fname] -> Right fname
      [] -> Left "no filename provided"
      _ -> Left "multiple files not supported"

eitherToError :: (Show a) => Either a b -> IO b
eitherToError = either (Exception.throwIO . IOError.userError . show) return
