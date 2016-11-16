{-# LANGUAGE FlexibleContexts #-}

module Syntax where

import Text.Parsec

import Control.Monad.Identity
import Data.Either

import Text.Printf
import Data.List

type VarName = String

data Type = NumT | BoolT | Func Type Type | PairT Type Type deriving (Eq, Show)
data Expr = Var VarName | App Expr Expr | Lam VarName Type Expr 
          | If Expr Expr Expr | LetExp VarName Expr Expr | LetRec VarName Expr Expr | AssignType Expr Type
          | T | F | Num Int 
          | Pair Expr Expr | UnopExp Unop Expr | BinopExp Binop Expr Expr
          | Zero  | Succ Expr deriving Eq

data Unop = Neg | Not | Fst | Snd deriving(Eq, Show)
data Binop = Plus | Minus | Times | Div | And | Or | Equals deriving(Eq, Show)
data Stmt = Let VarName Expr | Run Expr | LetR VarName Type Expr deriving Eq
type Program = [Stmt] 

instance Show Expr where
  show = showExpr 0
    where
      showExpr 0 e@(App _ _) =
        let es = collectApps e in
        intercalate " " $ map (showExpr 1) es
      showExpr 0 (Succ e) = printf "SUCC %s" (showExpr 1 e)
      showExpr 0 (Lam x t e) =
        let (e', vars) = collectVars e in
        printf "lambda %s: %s. %s" (intercalate " " (x:vars)) (show t) (showExpr 0 e')
      showExpr 0 e = showExpr 1 e
      showExpr 1 (Var x) = x
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

parseProgram :: String -> Either Text.Parsec.ParseError [Stmt]
parseProgram = runParser program () "stdin"

parseExpr :: String -> Expr
parseExpr s =
  case parseExpr' s of
    Right e -> e
    Left err -> error $ show err

parseExpr' :: String -> Either Text.Parsec.ParseError Expr
parseExpr' = runParser expr () "stdin"

omega = parseExpr "(lambda x:Bool. x x) (lambda x:Bool. x x)"

keywords = ["let","lambda"]
isKeyword x = x `elem` keywords

program :: Stream s m Char => ParsecT s () m Program
program = ws *> (stmt `sepEndBy1` symbol ";" <* ws <* eof)

stmt, letStmt :: Stream s m Char => ParsecT s () m Stmt
stmt = try letStmt <|> Run <$> expr
letStmt = Let <$> (kw "let" *> space *> identifier) <*> (symbol "=" *> expr)

expr, atom, lam, var :: Stream s m Char => ParsecT s () m Expr
expr = foldl1 App <$> (atom `sepEndBy1` ws)
atom = try lam <|> try var <|> parens expr
lam = do
  ids <- kw "lambda" *> space *> (tidentifier `sepBy1` ws)
  body <- symbol "." *> expr
  pure $ buildLambda body ids
var = Var <$> identifier
parens :: Stream s m Char => ParsecT s () m a -> ParsecT s () m a
parens = between (symbol "(") (symbol ")")


buildLambda :: Expr -> [(String,Type)] -> Expr
buildLambda body [] = body
buildLambda body (eyed:eyeds) = buildLambda (Lam (fst eyed) (snd eyed) body) eyeds

identifier :: Stream s m Char => ParsecT s () m String
identifier = do
  ws
  x <- (:) <$> letter <*> many (alphaNum <|> char '\'')
  if isKeyword x
  then unexpected $ "keyword in place of variable (" ++ x ++ ")"
  else pure x

typeParser :: Stream s m Char => ParsecT s () m Type
typeParser = ws *> ( pure (NumT) <* kw "int" <|> pure (BoolT) <* kw "bool" <|> Func <$> (typeParser <* kw "->") <*> typeParser)

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