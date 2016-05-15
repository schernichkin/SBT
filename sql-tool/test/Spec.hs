{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec where

import           SQL.Syntax
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck

concreteSyntaxTest :: Test
concreteSyntaxTest = testGroup "concrete syntax test"
  [ testGroup "printer"
    [ testCase "identifier" $
        let Right result = runStringPrinter identifier "user_profil_link"
        in result @?= "user_profil_link"
    , testCase "column" $
        let Right result = runStringPrinter column $ ColumnDef "contacts_id" "VARCHAR(20)"
        in result @?= "contacts_id VARCHAR(20)"
    ]
  ]

main :: IO ()
main = defaultMain
  [ concreteSyntaxTest
  ]
