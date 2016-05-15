{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Commands.CreateTableFromCSV
  ( readColumnInfos
  , readTableDefs
  ) where

import           Control.Monad
import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import           Data.Default
import qualified Data.Map.Strict      as M
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           Prelude              hiding (readFile)
import           SQL.Syntax.Abstract

data ColumnInfo = ColumnInfo
  { _tableName  :: !Text
  , _columnName :: !Text
  , _dataType   :: !Text
  } deriving ( Show )

instance FromNamedRecord ColumnInfo where
  parseNamedRecord m = ColumnInfo <$> m .: "TableName"
                                  <*> m .: "ColumnName"
                                  <*> m .: "DataType"

instance ToNamedRecord ColumnInfo where
  toNamedRecord ColumnInfo {..} = namedRecord
    [ "TableName"  .= _tableName
    , "ColumnName" .= _columnName
    , "DataType"   .= _dataType
    ]

instance DefaultOrdered ColumnInfo where
  headerOrder = const $ header
    [ "TableName"
    , "ColumnName"
    , "DataType"
    ]

readColumnInfos :: FilePath -> IO (Either String (Header, Vector ColumnInfo))
readColumnInfos path = decodeByName <$> BS.readFile path

readTableDefs ::  FilePath -> IO [TableDef]
readTableDefs path = do
  result <- readColumnInfos path
  case result of
    Right (_, columns) -> return $ map snd $ M.toList $ V.foldr addColumnInfo M.empty columns
    Left msg -> error msg
  where
    addColumnInfo ColumnInfo{..} tableMap =
      let columnDef = ColumnDef { _columnName = _columnName, _columnType = _dataType  }
          alterTableDef tableDef =
            fmap (\TableDef {..} -> TableDef
              { _tableName = _tableName
              , _tableColumns = columnDef : _tableColumns
              , _tableSerde = Just def
              , _tableStore = Just def
              } ) tableDef
            `mplus`
            Just TableDef
              { _tableName = _tableName
              , _tableColumns = [ columnDef ]
              , _tableSerde = Just def
              , _tableStore = Just def
              }
      in M.alter alterTableDef _tableName tableMap
