module Args
  ( execArgs
  ) where

import           Commands
import           Control.Monad
import           Options.Applicative
import           System.IO

withHelper :: Parser a -> ParserInfo a
withHelper p = info (p <* helper) fullDesc

createTablesSourceParser :: Parser CreateTablesSource
createTablesSourceParser = FromCSV
  <$> option str  ( short 'c' <> long "from-columns" <> metavar "FILE" )

createTablesParser :: Parser CreateTables
createTablesParser = CreateTables <$> createTablesSourceParser

argsParserInfo :: ParserInfo (IO ())
argsParserInfo = withHelper $ subparser
  ( command "create-tables" $ withHelper $ createTables <$> createTablesParser )

execArgs :: IO ()
execArgs = join $ execParser argsParserInfo
