 module Hw06 where

 import Control.Applicative
 import Data.Char
 import System.Exit
 import qualified Data.Map as Map
 import Data.Map (Map)
 import Data.Set (Set)
 import qualified Data.Set as Set

----------------------------------------------------------
--Parser things

 newtype Parser a = Parser { parse :: String -> Maybe (a,String) }
 instance Functor Parser where
   fmap f p = Parser $ \s -> (\(a,c) -> (f a, c)) <$> parse p s
 instance Applicative Parser where
   pure a = Parser $ \s -> Just (a,s)
   f <*> a = Parser $ \s ->
     case parse f s of
       Just (g,s') -> parse (fmap g a) s'
       Nothing -> Nothing
 instance Alternative Parser where
   empty = Parser $ \s -> Nothing
   l <|> r = Parser $ \s -> parse l s <|> parse r s
 ensure :: (a -> Bool) -> Parser a -> Parser a
 ensure p parser = Parser $ \s ->
    case parse parser s of
      Nothing -> Nothing
      Just (a,s') -> if p a then Just (a,s') else Nothing

 lookahead :: Parser (Maybe Char)
 lookahead = Parser f
   where f [] = Just (Nothing,[])
         f (c:s) = Just (Just c,c:s)

 satisfy :: (Char -> Bool) -> Parser Char
 satisfy p = Parser f
   where f [] = Nothing
         f (x:xs) = if p x then Just (x,xs) else Nothing
 eof :: Parser ()
 eof = Parser $ \s -> if null s then Just ((),[]) else Nothing
 char :: Char -> Parser Char
 char c = ws *> satisfy (==c)
 spaceChar :: Parser Char
 spaceChar = satisfy (==' ')
 str :: String -> Parser String
 str s = ws *> loop s
   where loop [] = pure []
         loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs
 parens :: Parser a -> Parser a
 parens p = (char '(' *> p) <* char ')'
 ws :: Parser ()
 ws = pure () <* many (satisfy isSpace)
 kw :: String -> Parser String
 kw s = ws *> ensure p (str s) where
    p x = x == s 
 keywords :: [String]
 keywords = ["lambda","let",". ", "="]

 isKeyword = (`elem` keywords)
 isValidVarChar :: Char -> Bool
 isValidVarChar c = (isAlphaNum c) || (c=='\'')

 var :: Parser String
 var = ws *>
       ensure (\c -> case c of (Just d) -> isAlpha d
                               _ -> False)
               lookahead *> ensure (not . isKeyword) (some (satisfy isValidVarChar))
----------------------------------------------------------------
--piazza question : WTF is up with pairs?
--makefiles in general 
 sepBy1 :: Parser a -> Parser b -> Parser [a]
 sepBy1 p sep = (:) <$> p <*> many (sep *> p) 

 type Varname = String
 data Exp = Var Varname | App Exp Exp | Lambda Varname Exp | ZeroValue | Succ Exp deriving (Eq, Show)

 atom, expr, app :: Parser Exp
 atom = (char '(' *> expr <* char ')') <|> Var <$> var 

 app =  App <$> ( atom <* spaceChar) <*> atom <|> atom
 expr = ws *> ((kw "lambda" *> lambdaParser )<|> foldl1 App <$> (sepBy1 app spaceChar) <|> atom)
 -- <|> Lambda <$> kw "lambda" *> var  <* (satisfy == '.') <*> expr

 lambdaParser :: Parser Exp
 lambdaParser = ws *> (Lambda <$> ( var <* (kw ". ")) <*> expr <|> Lambda <$> ( var) <*> lambdaParser)

 data Statement = Let Varname Exp | Expression Exp deriving (Show,Eq)
 type Program = [Statement] 

 letParser :: Parser Statement
 letParser = Let <$> ((kw "let" *> var) <* kw "=") <*> expr 

 statementParser :: Parser Statement
 statementParser = letParser <|> Expression <$> expr 

 program :: Parser Program
 program = ((sepBy1 statementParser (char ';')) <* (char ';')) <|> sepBy1 statementParser (char ';') <|> error "Parse error"

--parse program "let zero = lambda s z. z;let succ = lambda n. lambda s z. s (n s z);succ (succ zero)"

-------------------------------------------------------------------------
--we need to write  2) error detection, unbounded variable, unexpected keyword in place of var
 -- expecting waht molly said, parse error, eval errors 

 subst :: Exp -> Varname -> Exp -> Exp 
 subst (Var y) x e2 = if (x==y) then (e2) else Var y
 subst (App e1 e1') x e2 = App (subst e1 x e2) (subst e1' x e2)
 subst (Lambda y e1) x e2 = if (x==y) then (Lambda x e1) else Lambda y (subst e1 x e2)

 type Store = Map Varname Exp

 eval:: Exp ->Store -> Exp
 eval (Lambda x e) s = if ( (fv (Lambda x e) s) == Set.empty) 
                    then Lambda x e  
                    else error ("unbound variable " ++ (concat (Set.toAscList (fv (Lambda x e) s))))
                     
 eval (App e1 e2) s = eval (subst e1' x e2') s where
                    (Lambda x e1') = eval (e1) s
                    (e2') = eval (e2) s
 eval (Var x) s = case Map.lookup x s of
                (Just y) -> y
                Nothing -> error "unclosed expression" 
------ unbound 2 needs a case for eval a var???????
 peval:: Program -> Store -> (Program,Store)
 pevalAux:: Program -> Store -> (Program,Store) 
 peval l s = pevalAux (reverse l) s
 pevalAux [] s = ([],s)
 pevalAux ((Let x e):xs) s = (fst (pevalAux xs s'), snd (pevalAux xs s')) where s' = Map.insert x (eval e s) s

 fv :: Exp -> Store -> Set Varname
 fv (Var x) _ = Set.singleton x
 fv (Lambda x e) s = Set.difference (Set.difference (fv e s) (Set.singleton x)) (Map.keysSet s)
 fv (App e1 e2) s = Set.union (fv e1 s) (fv e2 s)
----------------------------------------------------------------------------
--Tests

 true', secnd :: Exp
 true' = (Lambda "n" (Lambda "x" (Var "n")))
 secnd = (Lambda "x" (Var "x"))
 omega = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))

 store = Map.fromList [("y", true'), ("z", secnd)]

 test1 = eval (App (App true' secnd) omega) (Map.empty) == Lambda "x" (Var "x") 

 unbound = Lambda "x" (Var "y")

 unbound2 = App (Lambda "x" (Var "y"))(Lambda "x" (Var "z"))


 ----------- office hours questions
 -- hw 5 - shuffle and main
 -- this, var case for eval??
 -- is parser error enough?
 -- any other errors???? (not unbound variables)

