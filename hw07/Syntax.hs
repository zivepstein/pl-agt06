
{-# LANGUAGE FlexibleContexts #-}

module Syntax where

import Text.Parsec
import Data.Char
import Control.Monad.Identity
import Data.Either

import Text.Printf
import Data.List

type VarName = String

data Type = NumT | BoolT | Func Type Type | PairT Type Type | Err String deriving Eq
data Expr = Var VarName | App Expr Expr | Lam VarName Type Expr 
          | If Expr Expr Expr | LetExp VarName Expr Expr | LetRec VarName Type Expr Expr | AssignType Expr Type
          | T | F | Num Int 
          | Pair Expr Expr | UnopExp Unop Expr | BinopExp Binop Expr Expr
          | Zero  | Succ Expr deriving Eq

data Unop = Neg | Not | Fst | Snd deriving Eq
data Binop = Plus | Minus | Times | Div | And | Or | Equals deriving Eq
data Stmt = Let VarName Expr | Run Expr | LetR VarName Type Expr deriving Eq
type Program = [Stmt] 

instance Show Expr where
  show = showExpr 0
    where
      showExpr 0 e@(App _ _) =
        let es = collectApps e in
        intercalate " " $ map (showExpr 1) es
      showExpr 0 (Succ e) = printf "SUCC %s" (showExpr 1 e)
      showExpr 0 (Lam x t e) = "<function>"
      --   let (e', vars) = collectVars e in
      --   printf "lambda %s. %s" (intercalate " " (( x ++ ":" ++ show t):vars))  (showExpr 0 e')
      showExpr 0 (If e1 e2 e3) = "if " ++ (show e1) ++ " then " ++ (show e2) ++ " else " ++ (show e3)
      showExpr 0 (LetExp v e1 e2) = "let " ++ v ++ "=" ++ (show e1) ++ " in " ++ (show e2)
      showExpr 0 T = "true"
      showExpr 0 F = "false"
      showExpr 0 (Num n) = show n 
      showExpr 0 (UnopExp u v) = show u ++ show v 
      showExpr 0 (BinopExp u e1 e2) = "(" ++ show e1 ++ ")" ++ show u ++ "(" ++ show e2 ++")"
      showExpr 0 (Pair e1 e2) = "(" ++ show e1 ++ "," ++ show e2 ++ ")"
      showExpr 0 (AssignType e t) = "(" ++ show e ++ ":" ++ show t ++ ")"
      showExpr 0 (LetRec x t e1 e2) = "let rec " ++ x ++ ":" ++ show t ++ "=" ++ show e1 ++ " in " ++ show  e2
      showExpr 0 e = showExpr 1 e
      showExpr 1 (Var x) = x 
      showExpr 1 Zero = "ZERO"
      showExpr 1 e = showExpr 2 e
      showExpr 2 e = printf "(%s)" (showExpr 0 e)
      showExpr _ e = error "bad precedence index for showExpr"
      collectApps (App e1 e2) = collectApps e1 ++ [e2]
      collectApps e = [e]
      collectVars (Lam x t e) = ((x ++ ":" ++ show t):) <$> collectVars e
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
  show (Err s) = "fucking stupid bullshit errRRRRRRRRR " ++ s

instance Show Unop where
  show Not = "not "
  show Neg = "- "
  show Fst = "fst "
  show Snd = "snd "

instance Show Binop where
  show Plus = "+"
  show Times = "*"
  show Div = "/"
  show And = "and "
  show Or = "or "
  show Equals = "=="
  show Minus = "-"

 

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

keywords = ["let","lambda", "if", "then", "else", "in", "let rec", "not", "fst", "snd", "true", "false", "or", "and"]
isKeyword x = x `elem` keywords

program :: Stream s m Char => ParsecT s () m Program
program = ws *> (stmt `sepEndBy1` symbol ";" <* ws <* eof)

stmt, letStmt :: Stream s m Char => ParsecT s () m Stmt
stmt = try letStmt <|> Run <$> expr 
letStmt = Let <$> (kw "let" *> space *> identifier) <*> (symbol "=" *> expr)

expr, atom, lam, var, ifParser, letExpParser,unopExprParser, intParser, pairParser, assignParser, letRecParser, boolParser:: Stream s m Char => ParsecT s () m Expr
expr = foldl1 App <$> (atom `sepEndBy1` ws)
atom = try binopExpParser <|> try unopExprParser <|> try lam <|> try var <|> try ifParser <|> try letExpParser 
        <|> try pairParser <|> try assignParser <|> try letRecParser <|> try boolParser <|> try intParser <|> parens expr 
lam = do
  ids <- try (kw "lambda" *> space *> try (many1 (try ((parens tidentifier)))))  <|>  try (kw "lambda" *> space *> try (many1 (try ( tidentifier)))) 
  body <- (symbol "." *> expr)
  pure $ buildLambda body ids
var = Var <$> identifier
parens :: Stream s m Char => ParsecT s () m a -> ParsecT s () m a
parens = between (symbol "(") (symbol ")")
ifParser = If <$> (kw "if" *> expr) <*> (kw "then" *> expr) <*> (kw "else" *> expr)
letExpParser = LetExp <$> ((kw "let" *> identifier) <* symbol "=" )<*> (expr <* kw "in" )<*> expr
unopExprParser = UnopExp <$> (ws *> unopParser) <*> (ws *> expr)
boolParser = try (pure T <* kw "true") <|> pure F <* kw "false"
intParser = Num <$> (ws *> (read <$> many1 (satisfy isDigit)))
pairParser = Pair <$> (try (symbol "(" *> expr)) <*> ((symbol "," *> try expr) <* symbol ")") 
assignParser = AssignType <$> (symbol "(" *> expr) <*> ((symbol ":" *> typeParser) <* symbol ")") 
letRecParser = LetRec <$> ((kw "let rec" *> identifier) <* symbol ":") <*> (typeParser <* symbol "=") <*> (expr <* kw "in") <*> expr

unopParser :: Stream s m Char => ParsecT s () m Unop
unopParser = try (pure Not <* kw "not")  <|> try (pure Neg <* symbol "-")  <|> try (pure Fst <* kw "fst")  <|> pure Snd <* kw "snd" 

binopExpParser, binopMul, binopFactor, binopAnd :: Stream s m Char => ParsecT s () m Expr
binopExpParser = binopAnd`chainl1` binopParserEq -- <|> binopFactor
binopAnd = binopMul `chainl1` binopParserAnd -- <|> binopMul
binopMul = binopFactor `chainl1` binopParserMul -- <|> binopFactor
binopFactor = try (parens binopExpParser) <|> try intParser <|> try var <|> try lam 
            <|> try (parens expr) <|> try pairParser <|> try unopExprParser <|>boolParser
--(symbol "+" pure (BinopExp Plus))
 -- (\e1 bi e2 -> BinopExp bi e1 e2) <$> expr <*> binopParserAnd <*> expr

binopParserAnd, binopParserMul, binopParserEq  :: Stream s m Char => ParsecT s () m (Expr -> Expr -> Expr)
binopParserAnd = try (return (BinopExp Plus) <* symbol "+") <|> try (return  (BinopExp Minus) <* symbol "-")
               <|> try (return (BinopExp Or) <* kw "or") 
binopParserMul =  try (do{ symbol "*"; return (BinopExp Times)   })
         <|> try (do{ symbol "/"; return (BinopExp Div) })  <|> try (do{ kw "and"; return (BinopExp And) })
binopParserEq =  try (do{ symbol "=="; return (BinopExp Equals)   })
  --return (BinopExp Times) <* symbol "*" <|> return (BinopExp Div )<* symbol "/" <|> return $ (BinopExp And) <* symbol "&&"


 
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
