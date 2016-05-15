
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Commands
  ( CreateTables (..)
  , CreateTablesSource (..)
  , createTables
  ) where

import           Commands.CreateTableFromCSV
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import qualified Data.Text                   as T
import           SQL.Syntax
import           SQL.Syntax.Lens
import           System.FilePath

data CreateTables = CreateTables
  { _createTablesSource   :: !CreateTablesSource
  , _defaultStoreLocation :: !(Maybe String)
  } deriving ( Show )

data CreateTablesSource = FromCSV FilePath deriving ( Show )

createTables ::  CreateTables -> IO ()
createTables CreateTables {..} = case _createTablesSource of
   FromCSV path -> do
     tableDefs <- readTableDefs path
     forM_ tableDefs $ \tableDef -> do
       let fixedDef = flip execState tableDef $ do
            name <- T.unpack <$> use tableName
            let defaultLocation = T.pack $ fromMaybe "" _defaultStoreLocation </> name
            tableStore . _Just . storeLocation %= flip mplus (Just defaultLocation)
       case runStringPrinter createTable fixedDef of
         Right a -> putStrLn a
         Left  e -> putStrLn $ show e ++ ": " ++ show tableDef
