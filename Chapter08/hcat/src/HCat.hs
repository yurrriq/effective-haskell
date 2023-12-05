{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import qualified Control.Exception as Exception
import Data.Text (Text)
import qualified Data.Text as Text
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

wordWrap :: Int -> Text -> [Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
      let (candidate, nextLines) = Text.splitAt lineLength lineText
          (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
       in firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardWrappedText textIndex
      | textIndex <= 0 = (hardWrappedText, Text.empty)
      | Text.index hardWrappedText textIndex == ' ' =
          let (wrappedLine, rest) = Text.splitAt textIndex hardWrappedText
           in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardWrappedText (textIndex - 1)
