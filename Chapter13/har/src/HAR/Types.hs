{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HAR.Types where

import Control.Applicative (Alternative)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.ByteString (ByteString)
import Data.Text (Text)

data Archive = Archive
  { archiveName :: Text,
    archivedFiles :: [ArchivedFile]
  }
  deriving stock (Show)

data ArchivedFile = ArchivedFile
  { archivedFileName :: Text,
    archivedFileContents :: ByteString
  }
  deriving stock (Show)

newtype Archiver a = Archiver
  {unArchiver :: StateT Text (ExceptT Text IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      Alternative,
      MonadState Text,
      MonadError Text,
      MonadIO
    )

runArchiver :: Text -> Archiver a -> IO (Either Text a)
runArchiver inputText archiver =
  runExceptT $ evalStateT (unArchiver archiver) inputText
