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

defaultLocationParser :: Parser String
defaultLocationParser = option str ( short 'l' <> long "default-location" <> metavar "FILE" )

createTablesParser :: Parser CreateTables
createTablesParser = CreateTables
                  <$> createTablesSourceParser
                  <*> optional defaultLocationParser

argsParserInfo :: ParserInfo (IO ())
argsParserInfo = withHelper $ subparser
  ( command "create-tables" $ withHelper $ createTables <$> createTablesParser )

execArgs :: IO ()
execArgs = join $ execParser argsParserInfo
