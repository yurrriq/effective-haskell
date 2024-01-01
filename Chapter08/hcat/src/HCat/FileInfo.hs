{-# LANGUAGE RecordWildCards #-}

module HCat.FileInfo
  ( FileInfo (..),
    fileInfo,
    formatFileInfo,
  )
where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified System.Directory as Directory
import Text.Printf (printf)

data FileInfo = FileInfo
  { filePath :: FilePath,
    fileSize :: Int,
    fileMTime :: Clock.UTCTime,
    fileReadable :: Bool,
    fileWritable :: Bool,
    fileExecutable :: Bool
  }
  deriving (Show)

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
