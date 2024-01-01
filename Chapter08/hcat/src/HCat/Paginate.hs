{-# LANGUAGE LambdaCase #-}

module HCat.Paginate
  ( getTerminalSize,
    paginate,
    showPages,
  )
where

import Control.Arrow (second)
import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import HCat.FileInfo
import System.IO
import qualified System.Info as SystemInfo
import System.Process (readProcess)
import Text.Read (readMaybe)
import Prelude hiding (lines)

data ContinueCancel
  = Continue
  | Cancel
  deriving (Eq, Show)

getContinue :: IO ContinueCancel
getContinue =
  do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    getChar >>= \case
      ' ' -> return Continue
      'q' -> return Cancel
      _ -> getContinue

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int,
    screenColumns :: Int
  }
  deriving (Show)

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other -> pure (ScreenDimensions 25 80)
  where
    tputScreenDimensions =
      ScreenDimensions
        <$> (fromMaybe 25 <$> tput ["lines"])
        <*> (fromMaybe 80 <$> tput ["cols"])
    tput args =
      Exception.handle ope $
        readMaybe . init <$> readProcess "tput" args ""
    ope :: IOError -> IO (Maybe Int)
    ope = const (pure Nothing)

paginate :: ScreenDimensions -> FileInfo -> Text -> [Text]
paginate (ScreenDimensions rows cols) finfo text =
  let rows' = rows - 1
      unwrappedLines = Text.lines text
      wrappedLines = concatMap (wordWrap cols) unwrappedLines
      pageLines = groupsOf rows' wrappedLines
      pages = map (Text.unlines . padTo rows') pageLines
      pageCount = length pages
      statusLines = map (formatFileInfo finfo cols pageCount) [1 .. pageCount]
   in zipWith (<>) pages statusLines
  where
    padTo lineCount rowsToPad = take lineCount (rowsToPad <> repeat " ")

showPages :: [Text] -> IO ()
showPages [] = return ()
showPages (page : pages) =
  do
    clearScreen
    TextIO.putStrLn page
    getContinue >>= \case
      Continue -> showPages pages
      Cancel -> return ()

clearScreen :: IO ()
clearScreen =
  BS.putStr "\^[[1J\^[[1;1H"

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n elems = uncurry (:) $ second (groupsOf n) $ splitAt n elems

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
