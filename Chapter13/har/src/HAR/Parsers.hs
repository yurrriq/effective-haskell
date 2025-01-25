{-# LANGUAGE OverloadedStrings #-}

module HAR.Parsers
  ( parseArchive,
  )
where

import Control.Applicative (many, (<|>))
import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, gets, put)
import qualified Data.ByteString as ByteString
import Data.Char (isSeparator)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Enc
import HAR.Types (Archive (..), ArchivedFile (..), Archiver)

parseArchive :: Archiver Archive
parseArchive =
  do
    expectText "archive" <* dropSpaces
    name <- quotedString <* expectText ":\n"
    files <- parseBlock parseArchiveStatements
    pure $ Archive name files

parseArchiveStatements :: Archiver [ArchivedFile]
parseArchiveStatements =
  many $ dropEmptyLines *> (parseImportStatement <|> parseNewFileStatement)

parseImportStatement :: Archiver ArchivedFile
parseImportStatement =
  do
    expectText "import"
    dropSpaces
    path <- quotedString
    dropSpaces
    expectChar '\n'
    contents <- liftIO $ ByteString.readFile (Text.unpack path)
    pure $ ArchivedFile path contents

parseNewFileStatement :: Archiver ArchivedFile
parseNewFileStatement =
  do
    expectText "new-file"
    dropSpaces
    path <- quotedString
    dropSpaces
    expectText ":\n"
    body <- Enc.encodeUtf8 <$> parseBlock get
    pure $ ArchivedFile path body

parseBlock :: Archiver a -> Archiver a
parseBlock blockParser =
  dropEmptyLines *> getBlock >>= runSubparser blockParser
  where
    getBlock =
      do
        firstLineSpacing <- takeUntil (not . isSeparator)
        let indentation = Text.length firstLineSpacing
        firstLine <- restOfLine
        restOfBlock <- many (dropEmptyLines *> parseIndentedLine indentation)
        pure $ Text.unlines (firstLine : restOfBlock)

parseChar :: Archiver Char
parseChar =
  gets Text.uncons
    >>= maybe (throwError "end of input") (\(c, rest) -> put rest $> c)

parseIndentedLine :: Int -> Archiver Text
parseIndentedLine indentLevel =
  do
    expectText $ Text.replicate indentLevel " "
    restOfLine

runSubparser :: Archiver a -> Text -> Archiver a
runSubparser action subparserState =
  do
    oldText <- get
    put subparserState
    result <- action
    put oldText
    pure result

quotedString :: Archiver Text
quotedString =
  do
    expectChar '"'
    quotedText <- takeUntil (== '"')
    expectChar '"'
    pure quotedText

expectText :: Text -> Archiver ()
expectText expected =
  gets (Text.stripPrefix expected)
    >>= maybe (throwError "missing expected string") put

restOfLine :: Archiver Text
restOfLine = remainderOfLine <|> remainderOfText
  where
    remainderOfLine =
      do
        txt <- takeUntil isNewline
        expectChar '\n'
        pure txt
    remainderOfText = get

expectChar :: Char -> Archiver ()
expectChar = expect parseChar

takeUntil :: (Char -> Bool) -> Archiver Text
takeUntil predicate =
  do
    (result, rest) <- gets (Text.break predicate)
    put rest
    pure result

isNewline :: Char -> Bool
isNewline = (== '\n')

expect :: (Eq a) => Archiver a -> a -> Archiver ()
expect getActual expected =
  do
    actual <- getActual
    when (expected /= actual) $
      throwError "expectation violated"

dropEmptyLines :: Archiver ()
dropEmptyLines =
  void $ many $ dropSpaces *> expectChar '\n'

dropSpaces :: Archiver ()
dropSpaces = void $ many (expectChar ' ')
