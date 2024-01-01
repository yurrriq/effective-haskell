{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import Control.Arrow (second)
import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified System.Directory as Directory
import qualified System.Environment as Env
import System.IO
import qualified System.IO.Error as IOError
import qualified System.Info as SystemInfo
import System.Process (readProcess)
import Text.Printf (printf)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int,
    screenColumns :: Int
  }
  deriving (Show)

data FileInfo = FileInfo
  { filePath :: FilePath,
    fileSize :: Int,
    fileMTime :: Clock.UTCTime,
    fileReadable :: Bool,
    fileWritable :: Bool,
    fileExecutable :: Bool
  }
  deriving (Show)

data ContinueCancel
  = Continue
  | Cancel
  deriving (Eq, Show)

runHCat :: IO ()
runHCat =
  withErrorHandling $
    do
      targetFilePath <- eitherToError =<< handleArgs
      contents <- TextIO.hGetContents =<< openFile targetFilePath ReadMode
      termSize <- getTerminalSize
      hSetBuffering stdout NoBuffering
      finfo <- fileInfo targetFilePath
      let pages = paginate termSize finfo contents
      showPages pages
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

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text
formatFileInfo FileInfo {..} maxWidth totalPages currentPage =
  let permissionString =
        [ if fileReadable then 'r' else '-',
          if fileWritable then 'w' else '-',
          if fileExecutable then 'x' else '-'
        ]
      timestamp =
        TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
      statusLine =
        Text.pack $
          printf
            "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
            filePath
            permissionString
            fileSize
            timestamp
            currentPage
            totalPages
   in invertText (truncateStatus statusLine)
  where
    invertText inputStr =
      let reverseVideo = "\^[[7m"
          resetVideo = "\^[[0m"
       in reverseVideo <> inputStr <> resetVideo
    truncateStatus statusLine
      | maxWidth <= 3 = ""
      | Text.length statusLine > maxWidth =
          Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine

fileInfo :: FilePath -> IO FileInfo
fileInfo path =
  do
    perms <- Directory.getPermissions path
    mtime <- Directory.getModificationTime path
    size <- BS.length <$> BS.readFile path
    return
      FileInfo
        { filePath = path,
          fileSize = size,
          fileMTime = mtime,
          fileReadable = Directory.readable perms,
          fileWritable = Directory.writable perms,
          fileExecutable = Directory.executable perms
        }

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
groupsOf _ [] = []
groupsOf n elems = uncurry (:) $ second (groupsOf n) $ splitAt n elems
