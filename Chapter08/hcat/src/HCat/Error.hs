{-# LANGUAGE TypeApplications #-}

module HCat.Error
  ( eitherToError,
    withErrorHandling,
  )
where

import qualified Control.Exception as Exception
import qualified Data.Text.IO as TextIO
import qualified System.IO.Error as IOError

eitherToError :: (Show a) => Either a b -> IO b
eitherToError = either (Exception.throwIO . IOError.userError . show) return

withErrorHandling :: IO () -> IO ()
withErrorHandling =
  Exception.handle $ \err ->
    TextIO.putStrLn "I ran into an error:" >> print @IOError err
