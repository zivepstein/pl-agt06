module Test where

import Syntax
import Eval
import Main
import Text.Parsec
import qualified Data.Map as Map
import Data.Map (Map)

import Test.QuickCheck

varName :: Gen String
varName = elements (map (:[]) ['a'..'z'])

unop :: Gen Unop 
unop = elements [Not, Fst, Snd, Neg]

binop :: Gen Binop
binop = elements [Times, Plus, Div, Minus, Equals, And, Or]

typeGen :: Gen Type
typeGen = elements [NumT , BoolT ]

instance Arbitrary Expr where
  arbitrary = sized $ expr
    where expr 0 = oneof [Var <$> varName,
                          (\x t -> Lam x t (Var x)) <$> varName <*> typeGen,
                          (\x t y -> Lam x t (Lam y t (Var x))) <$> varName <*> typeGen <*> varName,
                          (\x t y -> Lam x t (Lam y t (Var y))) <$> varName <*> typeGen <*> varName]
          expr n = oneof [Var <$> varName,
                          Lam <$> varName <*> typeGen <*> expr (n - 1),
                          App <$> expr (n `div` 2) <*> expr (n `div` 2), 
                          If <$> expr (n `div` 3) <*> expr (n `div` 3) <*> expr (n `div` 3),
                          UnopExp <$> unop <*> expr (n-1),
                          LetExp <$> varName <*> expr (n `div` 2) <*> expr (n `div` 2),
                           pure T, pure F,
                          LetRec  <$> varName <*> typeGen <*> expr (n `div` 2) <*> expr (n `div` 2),
                          AssignType <$> expr (n-1) <*> typeGen, 
                          Pair <$> expr (n `div` 2) <*> expr (n `div` 2),
                          BinopExp <$> binop <*> expr (n `div` 2) <*> expr (n `div` 2)
                          ]

instance Arbitrary Stmt where
  arbitrary = oneof [Let <$> varName <*> arbitrary, Run <$> arbitrary]

prop_prettyParse :: Expr -> Property
prop_prettyParse e = parseExpr' (show e) === Right e

prop_prettyParsePretty :: Expr -> Property
prop_prettyParsePretty e = show (parseExpr (show e)) === show e

ctxt = Map.fromList[("x", NumT), ("y", NumT), ("b", BoolT)] :: G

expr1 :: Expr
expr1 = Lam "x" NumT (T)
expr2 = App expr1 (Num 9)
expr3 = LetExp "x" expr2 (App (Lam "y" BoolT (Var "y")) (Var "x"))
expr4 = UnopExp Not expr3
expr5 = App (Lam "x" NumT (UnopExp Neg (Var "x"))) (Num 9)
expr6 = BinopExp Plus expr5 (Num 13) --4
expr7 = BinopExp Times expr6 (Num 2) --8
expr8 = BinopExp Div expr7 expr6 --2
expr9 = BinopExp Plus expr5 (Num 13)
expr12 = Pair expr7 T
expr13 = UnopExp Fst expr12
---Badly typed expressions
expr10 = UnopExp Not (Num 3)
expr11 = App (Lam "x" BoolT (BinopExp Plus (Var "x") (Num 3))) (Num 4)


test1:: Bool
test1 = eval expr4 == Right F
test2 = eval expr6 == Right (Num 4)
test3 = eval expr7 == Right (Num 8)
test4 = eval expr8 == Right (Num 2)
test5 = eval expr12 == Right (Pair (Num 8) T)
test6 = eval expr13 == Right (Num 8)

testLR1 = "let rec toZero: int->int = (lambda n:int. if (n==0) then 0 else (toZero (n-1))) in (toZero 5)"


tests = [test1, test2, test3, test4, test5, test6]



--Questions:  let rec evaluating, not show functions


--Todo: gut c and n, add unsafe flag, more tests, fact 5, not show functions (change level of show instance for lambda)



