module HAR.Example where

import Data.Text (Text)
import qualified Data.Text as Text
import HAR.Parsers (parseArchive)
import HAR.Types (Archive, runArchiver)
import Paths_har (getDataFileName)
import Text.Printf (printf)

example :: IO (Either Text Archive)
example =
  do
    example1 <- getDataFileName "data/example1.txt"
    example2 <- getDataFileName "data/example2.txt"
    runArchiver
      ( Text.pack $
          printf
            "archive \"example.archive\":\n\
            \\n\
            \  import \"%s\"\n\
            \\n\
            \  new-file  \"inline-example.txt\":\n\
            \    this is some text\n\
            \    it can have newlines\n\
            \    but it needs to be indented\n\
            \\n\
            \  import \"%s\"\n\
            \"
            example1
            example2
      )
      parseArchive
