{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}

module CSVConverter.Main
  ( defaultMain
  ) where

import           Control.Monad
import           CSVConverter.FancyCSV
import           Options.Applicative

withHelper :: Parser a -> ParserInfo a
withHelper p = info (p <* helper) fullDesc

toCSVArgsParser :: Parser ToCSVArgs
toCSVArgsParser = ToCSVArgs
  <$> option str ( short 'i' <> long "in" <> metavar "FILE" )
  <*> option str ( short 'o' <> long "out" <> metavar "FILE" )

argsParser :: ParserInfo (IO ())
argsParser = withHelper $ subparser
    ( command "to-csv" $ withHelper $ toCSV <$> toCSVArgsParser )

defaultMain :: IO ()
defaultMain = join $ execParser argsParser
