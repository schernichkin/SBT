{-# LANGUAGE OverloadedStrings #-}

module SQL.Syntax.Abstract
  ( ColumnDef  (..)
  , TableDef (..)
  , SerdeDef (..)
  , StoreDef (..)
  ) where

import           Data.Default
import           Data.Text

data ColumnDef = ColumnDef
  { _columnName :: !Text
  , _columnType :: !Text
  } deriving ( Show )

data SerdeDef = SerdeDef
  { _serdeClass      :: !Text
  , _serdeProperties :: !(Maybe [(Text, Text)])
  } deriving ( Show )

instance Default SerdeDef where
  def = SerdeDef
    { _serdeClass = "org.apache.hadoop.hive.serde2.lazy.LazySimpleSerDe"
    , _serdeProperties = Just
      [ ("serialization.encoding", "cp1251")
      , ("escape.delim", "\\")
      , ("quote.delim", "\"")
      , ("field.delim", "\t")
      ]
    }

data StoreDef = StoreDef
  { _storeInputFormat  :: !(Maybe Text)
  , _storeOutputFormat :: !(Maybe Text)
  , _storeLocation     :: !(Maybe Text)
  } deriving ( Show )

instance Default StoreDef where
  def = StoreDef
    { _storeInputFormat  = Just "org.apache.hadoop.mapred.TextInputFormat"
    , _storeOutputFormat = Just "org.apache.hadoop.hive.ql.io.HiveIgnoreKeyTextOutputFormat"
    , _storeLocation = Nothing
    }

data TableDef = TableDef
  { _tableName    :: !Text
  , _tableColumns :: ![ColumnDef]
  , _tableSerde   :: !(Maybe SerdeDef)
  , _tableStore   :: !(Maybe StoreDef)
  } deriving ( Show )
