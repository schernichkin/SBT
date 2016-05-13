
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Commands
  ( CreateTables (..)
  , CreateTablesSource (..)
  , createTables
  ) where

import           Commands.CreateTableFromCSV
import           Control.Monad
import           Data.Default
import           SQL.Syntax

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
       -- let fixed = case  of 
       case runStringPrinter createTable tableDef of
         Right a -> putStrLn a
         Left  e -> putStrLn $ (show e) ++ ": " ++ (show tableDef)
