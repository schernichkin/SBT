{-# LANGUAGE TemplateHaskell #-}

module SQL.Syntax.Lens where

import           Control.Lens.TH
import           SQL.Syntax.Abstract

makeLenses ''ColumnDef
makeLenses ''TableDef
makeLenses ''SerdeDef
makeLenses ''StoreDef
