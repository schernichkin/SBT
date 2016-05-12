module Main where

import           Control.Monad
import           Options.Applicative


data Options = Options
  { _optTableDefFromCSV :: TableDefFromCSV
  } deriving ( Show )

data TableDefFromCSV = TableDefFromCSV deriving ( Show )

opts :: Parser (IO ())
opts = subparser
  ( command "table-def-from-csv" (info (tableDefFromCsv <$> argument str idm) idm))

tableDefFromCsv dd = putStrLn dd

main :: IO ()
main = join $ execParser (info opts idm)
