{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import Control.Arrow (second, (>>>))
import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Environment as Env
import System.IO
import qualified System.IO.Error as IOError
import qualified System.Info as SystemInfo
import System.Process (readProcess)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int,
    screenColumns :: Int
  }
  deriving (Show)

data ContinueCancel
  = Continue
  | Cancel
  deriving (Eq, Show)

runHCat :: IO ()
runHCat =
  withErrorHandling $
    handleArgs
      >>= eitherToError
      >>= flip openFile ReadMode
      >>= TextIO.hGetContents
      >>= \contents ->
        getTerminalSize >>= \termSize ->
          let pages = paginate termSize contents
           in showPages pages
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

showPages :: [Text] -> IO ()
showPages [] = return ()
showPages (page : pages) =
  clearScreen
    >> TextIO.putStrLn page
    >> getContinue
    >>= \case
      Continue -> showPages pages
      Cancel -> return ()

clearScreen :: IO ()
clearScreen =
  BS.putStr "\^[[1J\^[[1;1H"

getContinue :: IO ContinueCancel
getContinue =
  hSetBuffering stdin NoBuffering
    >> hSetEcho stdin False
    >> getChar
    >>= \case
      ' ' -> return Continue
      'q' -> return Cancel
      _ -> getContinue

paginate :: ScreenDimensions -> Text -> [Text]
paginate (ScreenDimensions rows cols) text =
  let unwrappedLines = Text.lines text
      wrappedLines = concatMap (wordWrap cols) unwrappedLines
      pageLines = groupsOf rows wrappedLines
   in Text.unlines <$> pageLines

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other -> pure (ScreenDimensions 25 80)
  where
    tputScreenDimensions = ScreenDimensions <$> tput ["lines"] <*> tput ["cols"]
    tput = fmap (read . init) . flip (readProcess "tput") ""

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

groupsOf :: Int -> [a] -> [[a]]
groupsOf n = splitAt n >>> second (groupsOf n) >>> uncurry (:)
