{-# LANGUAGE LambdaCase #-}

module HCat where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import HCat.Error (eitherToError, withErrorHandling)
import HCat.FileInfo (fileInfo)
import HCat.Paginate (getTerminalSize, paginate, showPages)
import qualified System.Environment as Env
import System.IO

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

handleArgs :: IO (Either Text FilePath)
handleArgs = parseArgs <$> Env.getArgs
  where
    parseArgs = \case
      [fname] -> Right fname
      [] -> Left "no filename provided"
      _ -> Left "multiple files not supported"
