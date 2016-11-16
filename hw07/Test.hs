module Test where

import Syntax
import Text.Parsec

import Test.QuickCheck

varName :: Gen String
varName = elements (map (:[]) ['a'..'z'])

typeGen :: Gen Type
typeGen = elements [NumT , BoolT ]

instance Arbitrary Expr where
  arbitrary = sized $ expr
    where expr 0 = oneof [Var <$> varName]
                          -- (\x t -> Lam x t (Var x)) <$> varName <*> typeGen,
                          -- (\x t y -> Lam x t (Lam y t (Var x))) <$> varName <*> typeGen <*> varName,
                          -- (\x t y -> Lam x t (Lam y t (Var y))) <$> varName <*> typeGen <*> varName]
          expr n = oneof [Var <$> varName,
                          (\x t e -> Lam x t e) <$> varName <*> typeGen <*> expr (n - 1),
                          App <$> expr (n `div` 2) <*> expr (n `div` 2)]

instance Arbitrary Stmt where
  arbitrary = oneof [Let <$> varName <*> arbitrary, Run <$> arbitrary]

prop_prettyParse :: Expr -> Property
prop_prettyParse e = parseExpr' (show e) === Right e

prop_prettyParsePretty :: Expr -> Property
prop_prettyParsePretty e = show (parseExpr (show e)) === show e
