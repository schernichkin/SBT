{-# LANGUAGE DeriveGeneric #-}

module Commands
  ( CreateTables (..)
  , CreateTablesSource (..)
  , createTables
  ) where

import Data.Text
import GHC.Generics

data CreateTables = CreateTables
  { _createTablesSource :: CreateTablesSource
  } deriving ( Show )

data CreateTablesSource = FromCSV FilePath deriving ( Show )

data ColumnInfo = ColumnInfo
  { _table :: !Text
  , _column :: !Text
  , _type :: !Text
  } deriving ( Show, Generic )

createTables ::  CreateTables -> IO ()
createTables = print
