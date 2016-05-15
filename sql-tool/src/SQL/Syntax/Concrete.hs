{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards  #-}

module SQL.Syntax.Concrete
( -- * Syntax (SQL)
  column
, identifier
, singleLineComment
, createTable
-- * Printers
, runStringPrinter
) where

import           Text.Syntax.Poly
import  Data.Text ( Text )
import qualified Data.Text as T
import Control.Isomorphism.Partial.Ext
import Prelude hiding ( (<$>), (*>), (.), (<*), (<*>), words )
import qualified Prelude as P
import Data.Char
import Text.Syntax.Printer.List
import SQL.Syntax.Abstract
import           Data.Default

isLineSeparator :: Char -> Bool
isLineSeparator = (LineSeparator ==) . generalCategory

-- * Isomorphisms

stringTextIso :: Iso String Text
stringTextIso = iso T.pack T.unpack

-- * Parsers
-- ** General

text :: (Syntax Char delta) => Text -> delta ()
text = list . T.unpack

words :: (Syntax Char delta) => [Text] -> delta ()
words [] = syntax ()
words (w:ws) = text w <* go ws
  where go []     = syntax ()
        go (x:xs) = sepSpace <* text x <* go xs

-- ** SQL

escapeMap :: [(Char, Char)]
escapeMap =  [('b', '\b'), ('n', '\n'),
              ('f', '\f'), ('r', '\r'),
              ('t', '\t'), ('\\', '\\'),
              ('\"', '\"')]

escape :: (Syntax Char delta) => delta Char
escape =  choice $ map (uncurry decode) escapeMap
  where decode c r = this c *> syntax r

sqlChar :: (Syntax Char delta) => delta Char
sqlChar =  (this '\\' *> escape)
       <|> (subset (`notElem` ("\'\\" :: String)) <$> token)

string :: (Syntax Char delta) => delta Text
string = stringTextIso
      <$> between (this '\'') (this '\'') (many sqlChar)
      <|> syntaxError "String"

singleLineComment :: (Syntax Char delta) => delta Text
singleLineComment = stringTextIso
                 <$> list "--"
                  *> optSpace
                  *> many commentChar
                 <* (ignore '\n' <$> token)
  where
    commentChar = subset (P.not . isLineSeparator) <$> token

identifier :: (Syntax Char delta) => delta Text
identifier = stringTextIso
          <$> many identChar
          <|> syntaxError "Identifier"
  where
    identChar = subset (\c -> isAlphaNum c || c == '_') <$> token

column ::  (Syntax Char delta) => delta ColumnDef
column = iso f g
         <$> identifier
         <*> (sepSpace *> (stringTextIso <$> many columnDefChar))
  where
    f = uncurry ColumnDef
    g ColumnDef {..} = (_columnName, _columnType)
    columnDefChar = subset (',' /=) <$> token

columnDefs :: (Syntax Char delta) => delta [ColumnDef]
columnDefs = column `sepBy` (this ',' *> optSpace)

serdeProperty :: (Syntax Char delta) => delta (Text, Text)
serdeProperty = string <* (optSpace <* this '=' <* optSpace) <*> string

serdeProperties :: (Syntax Char delta) => delta [(Text, Text)]
serdeProperties = serdeProperty `sepBy` (this ',' *> optSpace)

serde :: (Syntax Char delta) => delta SerdeDef
serde = iso f g
     <$> words ["row", "format", "serde"]
      *> sepSpace
      *> string
     <*> optional (sepSpace *> words ["with", "properties"] *> optSpace *> properties)
  where
    f (sc, sp) = SerdeDef { _serdeClass = sc, _serdeProperties = sp }
    g SerdeDef { .. } = (_serdeClass, _serdeProperties)
    properties = (this '(' *> skipSpace)
                 *> serdeProperties
                <* (skipSpace <* this ')')

store :: (Syntax Char delta) => delta StoreDef
store = iso f g
     <$> words ["stored", "as"]
      *> (optional (sepSpace *> text "inputformat" *> sepSpace *> string))
     <*> (optional (sepSpace *> text "outputformat" *> sepSpace *> string))
     <*> (optional (sepSpace *> text "location" *> sepSpace *> string))
  where
    f (inf, (outf, loc)) = StoreDef
      { _storeInputFormat  = inf
      , _storeOutputFormat = outf
      , _storeLocation     = loc
      }
    g StoreDef {..} = (_storeInputFormat, (_storeOutputFormat, _storeLocation))

createTable :: (Syntax Char delta) => delta TableDef
createTable = iso f g
           <$> (words ["create", "table"])
            *> (sepSpace *> tableName)
           <*> (optSpace *> tablecolumns)
           <*> (optSpace *> optional serde)
           <*> (optSpace *> optional store)
  where
    f (name, (cols, (ser, stor))) =
      TableDef
        { _tableName = name
        , _tableColumns = cols
        , _tableSerde = ser
        , _tableStore = stor
        }
    g TableDef {..} = (_tableName, (_tableColumns, (_tableSerde, _tableStore)))
    tableName = identifier
    tablecolumns = (this '(' *> skipSpace)
                 *> columnDefs
                <* (skipSpace <* this ')')

-- * Printers

runStringPrinter :: RunAsStringPrinter a ErrorString
runStringPrinter =  runAsPrinter
