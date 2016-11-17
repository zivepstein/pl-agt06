{-# LANGUAGE FlexibleContexts #-}

module Syntax where

import Text.Parsec
import Data.Char
import Control.Monad.Identity
import Data.Either

import Text.Printf
import Data.List

type VarName = String

data Type = NumT | BoolT | Func Type Type | PairT Type Type deriving Eq
data Expr = Var VarName | App Expr Expr | Lam VarName Type Expr 
          | If Expr Expr Expr | LetExp VarName Expr Expr | LetRec VarName Type Expr Expr | AssignType Expr Type
          | T | F | Num Int 
          | Pair Expr Expr | UnopExp Unop Expr | BinopExp Binop Expr Expr
          | Zero  | Succ Expr deriving Eq

data Unop = Neg | Not | Fst | Snd deriving Eq
data Binop = Plus | Minus | Times | Div | And | Or | Equals deriving(Eq)
data Stmt = Let VarName Expr | Run Expr | LetR VarName Type Expr deriving Eq
type Program = [Stmt] 

instance Show Expr where
  show = showExpr 0
    where
      showExpr 0 e@(App _ _) =
        let es = collectApps e in
        intercalate " " $ map (showExpr 1) es
      showExpr 0 (Succ e) = printf "SUCC %s" (showExpr 1 e)
      showExpr 0 (Lam x t e) = "lambda " ++ x ++ ":" ++ (show t) ++ ". " ++ (show e)
        -- let (e', vars) = collectVars e in
        -- printf "lambda %s: %s. %s" (intercalate " " (x:vars)) (show t) (showExpr 0 e')
      showExpr 0 (If e1 e2 e3) = "if " ++ (show e1) ++ " then " ++ (show e2) ++ " else " ++ (show e3)
      showExpr 0 (LetExp v e1 e2) = "let " ++ v ++ "=" ++ (show e1) ++ " in " ++ (show e2)
      showExpr 0 T = "true"
      showExpr 0 F = "false"
      showExpr 0 (Num n) = show n
      showExpr 0 (UnopExp u v) = show u ++ show v 
      showExpr 0 (BinopExp u e1 e2) = show e1 ++ show u ++ show e2
      showExpr 0 (Pair e1 e2) = "(" ++ show e1 ++ "," ++ show e2 ++ ")"
      showExpr 0 (AssignType e t) = "(" ++ show e ++ ":" ++ show t ++ ")"
      showExpr 0 (LetRec x t e1 e2) = "let rec " ++ x ++ ":" ++ show t ++ "=" ++ show e1 ++ " in " ++ show  e2
      showExpr 0 e = showExpr 1 e
      showExpr 1 (Var x) = "^" ++ x
      showExpr 1 Zero = "ZERO"
      showExpr 1 e = showExpr 2 e
      showExpr 2 e = printf "(%s)" (showExpr 0 e)
      showExpr _ e = error "bad precedence index for showExpr"
      collectApps (App e1 e2) = collectApps e1 ++ [e2]
      collectApps e = [e]
      collectVars (Lam x t e) = (x:) <$> collectVars e
      collectVars e = (e,[])

instance Show Stmt where
  show (Let x e) = printf "let %s = %s;" x (show e)
  show (Run e) = printf "%s;" (show e)
-- parser, using Parsec

instance Show Type where
  show BoolT = "bool"
  show NumT = "int"
  show (Func t1 t2) = show t1 ++ "->" ++ show t2
  show (PairT t1 t2) = printf "(%s,%s)" (show t1) (show t2)

instance Show Unop where
  show Not = "not "
  show Neg = "-"
  show Fst = "fst "
  show Snd = "snd "

instance Show Binop where
  show Plus = "+"
  show Times = "*"
  show Div = "/"
  show Minus = "-"
  show And = "&&"
  show Or = "||"
  show Equals = "=="


parseProgram :: String -> Either Text.Parsec.ParseError [Stmt]
parseProgram = runParser program () "stdin"

parseExpr :: String -> Expr
parseExpr s =
  case parseExpr' s of
    Right e -> e
    Left err -> error $ show err

parseExpr' :: String -> Either Text.Parsec.ParseError Expr
parseExpr' = runParser expr () "stdin"

omega = parseExpr "(lambda x:bool. x x) (lambda x:bool. x x)"

keywords = ["let","lambda", "if", "then", "else", "in", "not", "fst", "snd", "true", "false", "rec", "let rec"]
isKeyword x = x `elem` keywords

program :: Stream s m Char => ParsecT s () m Program
program = ws *> (stmt `sepEndBy1` symbol ";" <* ws <* eof)

stmt, letStmt :: Stream s m Char => ParsecT s () m Stmt
stmt = try letStmt <|> Run <$> expr 
letStmt = Let <$> (kw "let" *> space *> identifier) <*> (symbol "=" *> expr)

expr, atom, lam, var, ifParser, letExpParser,unopExprParser, boolParser, intParser, pairParser, assignParser,letRecParser :: Stream s m Char => ParsecT s () m Expr
expr = foldl1 App <$> (atom `sepEndBy1` ws)
atom = try lam <|> try var <|> try ifParser <|> try letExpParser 
       <|> try unopExprParser <|> try boolParser <|> try intParser <|> try pairParser <|> try assignParser <|> try letRecParser<|> parens expr 
lam = do
  ids <- try (kw "lambda" *> space *> ((parens tidentifier) `sepBy1` ws))  <|> kw "lambda" *> space *> ( tidentifier `sepBy1` ws)   
  body <- symbol "." *> ws *> expr
  pure $ buildLambda body ids
var = Var <$> identifier
parens :: Stream s m Char => ParsecT s () m a -> ParsecT s () m a
parens = between (symbol "(") (symbol ")")
ifParser = If <$> (kw "if" *> expr) <*> (kw "then" *> expr) <*> (kw "else" *> expr)
letExpParser = LetExp <$> ((kw "let" *> identifier) <* symbol "=" )<*> (expr <* kw "in" )<*> expr
unopExprParser = UnopExp <$> unopParser <*> expr
boolParser = pure T <* kw "true" <|> pure F <* kw "false"
intParser = Num <$> (ws *> (read <$> many1 (satisfy isDigit)))
pairParser = Pair <$> (symbol "(" *> expr) <*> ((symbol "," *> expr) <* symbol ")") 
assignParser = AssignType <$> (symbol "(" *> expr) <*> ((symbol ":" *> typeParser) <* symbol ")") 
letRecParser = LetRec <$> ((kw "let rec" *> identifier) <* symbol ":") <*> (typeParser <* symbol "=") <*> (expr <* kw "in") <*> expr
-- binopExpParser =  binopMul `chainl1` binopParserAnd
-- binopMul = binopFactor `chainl1` binopParserMul
-- binopFactor = parens binopExpParser <|> expr
-- (symbol "+" pure (BinopExp Plus))
  -- (\e1 bi e2 -> BinopExp bi e1 e2) <$> expr <*> binopParserAnd <*> expr

unopParser :: Stream s m Char => ParsecT s () m Unop
unopParser = pure Not <* kw "not"  <|> pure Neg <* symbol "-"  <|> pure Fst <* kw "fst"  <|> pure Snd <* kw "snd" 

-- binopParserAnd, binopParserMul  :: Stream s m Char => ParsecT s () m Binop
-- binopParserAnd = pure $ BinopExp Plus <* symbol "+" <|> pure $ BinopExp Minus <* symbol "-" 
--                <|> pure $ BinopExp Or <* symbol "||" <|> pure $ BinopExp Equals <* symbol "=="
-- binopParserMul = pure $ BinopExp Times <* symbol "*" <|> pure $ BinopExp Div <* symbol "/" <|> pure $ BinopExp And <* symbol "&&"

buildLambda :: Expr -> [(String,Type)] -> Expr
buildLambda body [] = body
buildLambda body (eyed:eyeds) =  (Lam (fst eyed) (snd eyed) (buildLambda body eyeds)) 

identifier :: Stream s m Char => ParsecT s () m String
identifier = do
  ws
  x <- (:) <$> letter <*> many (alphaNum <|> char '\'')
  if isKeyword x
  then unexpected $ "keyword in place of variable (" ++ x ++ ")"
  else pure x

typeParser, typeAtom :: Stream s m Char => ParsecT s () m Type
typeParser = ws *> (try (Func <$> (typeAtom <* symbol "->") <*> typeParser) <|> try (PairT <$> ((symbol "(" *> typeAtom) <* symbol ",") <*> (typeParser <* symbol ")")) <|> typeAtom)
typeAtom = ws *>  (pure (NumT) <* symbol "int" <|> pure (BoolT) <* symbol "bool")

tidentifier :: Stream s m Char => ParsecT s () m (String,Type)
tidentifier = (\a b -> (a,b)) <$> identifier <*> (symbol ":" *> typeParser)

kw :: Stream s m Char => String -> ParsecT s () m ()
kw s = symbol s *> notFollowedBy alphaNum

symbol :: Stream s m Char => String -> ParsecT s () m String
symbol s = ws *> string s

ws, comment :: Stream s m Char => ParsecT s () m ()
ws = many (try comment <|> space *> pure ()) *> pure ()
comment = string "--" *> manyTill anyChar eol *> pure ()
  where eol = (try endOfLine *> pure ()) <|> eof