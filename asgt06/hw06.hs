 module Hw06 where

 import Control.Applicative
 import Data.Char
 import System.Exit
 import qualified Data.Map as Map
 import Data.Map (Map)
 import Data.Set (Set)
 import qualified Data.Set as Set
 import System.Exit
 import System.Environment
 import System.Exit
 import System.IO
 import Data.Array.IO
 import Control.Monad

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
 peval [] s = ([],s)
 peval ((Let x e):xs) s = (fst pA, snd pA) where pA = peval xs (Map.insert x (eval e s) s)
 peval (Expression e:xs) s = (Expression (eval e s): (fst pA), snd pA) where pA = peval xs s

 fv :: Exp -> Store -> Set Varname
 fv (Var x) _ = Set.singleton x
 fv (Lambda x e) s = Set.difference (Set.difference (fv e s) (Set.singleton x)) (Map.keysSet s)
 fv (App e1 e2) s = Set.union (fv e1 s) (fv e2 s)




----------------------------------------------------------------------------
--Tests

 true', secnd, false' :: Exp
 true' = (Lambda "n" (Lambda "x" (Var "n")))
 false' = (Lambda "n" (Lambda "x" (Var "x")))
 --and' = (Lambda "x" (Lambda "y" (App(App((Var "x") (Var "y")) false'))))
 secnd = (Lambda "x" (Var "x"))
 omega = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))

 store = Map.fromList [("z", true'), ("s", secnd)]


 test1 = eval (App (App true' secnd) omega) (Map.empty) == Lambda "x" (Var "x") 

 unbound = Lambda "x" (Var "y")

 unbound2 = App (Lambda "x" (Var "y"))(Lambda "x" (Var "z"))

 p1 = [Let "zero" (Lambda "s" (Lambda "z" (Var "z"))),Let "succ" (Lambda "n" (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))))),Expression (App (Var "succ") (App (Var "succ") (Var "zero"))),Expression (App (Var "succ") (App (Var "succ") (App (Var "succ") (Var "zero"))))]
 p1out = [Expression (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Lambda "s" (Lambda "z" (Var "z"))) (Var "s")) (Var "z"))))) (Var "s")) (Var "z"))))),Expression (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Lambda "s" (Lambda "z" (Var "z"))) (Var "s")) (Var "z"))))) (Var "s")) (Var "z"))))) (Var "s")) (Var "z")))))]

 p2 = [Let "zero" (Lambda "s" (Lambda "z" (Var "z"))),Let "succ" (Lambda "n" (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))))),Expression (App (Var "succ") (App (Var "succ") (Var "zero")))]

 pevalTest1 = case (parse program "lambda s z. s ((lambda s z. s ((lambda s z. z) s z)) s z); lambda s z. s ((lambda s z. s ((lambda s z. s ((lambda s z. z) s z)) s z)) s z)") of 
              (Just x) -> fst x == fst(peval p1 Map.empty)
              Nothing -> error "whoops"

 pevalTest2 = case (parse program "lambda s z. s ((lambda s z. s ((lambda s z. z) s z)) s z)") of 
              (Just x) -> fst x == fst(peval p2 Map.empty)
              Nothing -> error "whoops"

 true = Expression $ Lambda "n" ((Lambda "x" (Var "n"))) 
 false =  Expression $ Lambda "n" ((Lambda "x" (Var "x")))
 notBool =  Expression $ Lambda "b" (Lambda "x" (Lambda "y" (App (App (Var "b") (Var "x")) (Var "y"))))
 notFalse = Expression $ App false' (Lambda "b" (Lambda "x" (Lambda "y" (App (App (Var "b") (Var "x")) (Var "y")))))
 andBoolExp = (Lambda "x" (Lambda "y" (App(App (Var "x") (Var "y")) false')))
 trueAndFalseProg = Expression $ App (App andBoolExp (true')) false'
 booltest1 = peval [trueAndFalseProg] Map.empty == peval [false] Map.empty

 main :: IO ()
 main = do
          args <- getArgs
          result <-  (readAndEvalFile (head args))
          putStrLn (prettyPrint $ (map statementToExpr result))
          -- putStrLn mainAux args    

 -- mainAux :: [String] -> Program
 -- mainAux [] = do 
 --                split <- (splitOn "\n") <$> getContents
 --                shuf <- fastShuffle split 
 --                return $ intercalate "\n" shuf 
 -- mainAux ("-":xs) = do 
 --                split <- (splitOn "\n") <$> getContents
 --                shuf <- fastShuffle split 
 --                return $ intercalate "\n" shuf 
 -- mainAux [arg] = readAndEvalFile arg
 -- mainAux _ = die "you wrong"


 parsed :: Maybe (Program,String) -> Program
 parsed (Just x) = fst $ peval (fst x) Map.empty
 parsed Nothing = error "We cannot ever get here - so parse failed?"

 readAndEvalFile :: FilePath -> IO Program
 readAndEvalFile f = do
                    contents <-  readFile f
                    return $ parsed (parse program contents)

 prettyPrint :: [Exp] -> String
 prettyPrint ((Lambda a x):ps) = "lambda " ++ a ++ ". ("++(prettyPrint [x]) ++") " ++ (prettyPrint ps)
 prettyPrint ((App e1 e2):ps) = "(" ++ prettyPrint [e1] ++ ") ("++ prettyPrint [e2] ++ (prettyPrint ps) ++")"
 prettyPrint ((Var x):ps) = x ++ (prettyPrint ps)
 prettyPrint [] = ""

 statementToExpr :: Statement -> Exp 
 statementToExpr (Let x e) = e
 statementToExpr (Expression e) = e

 --readAndEvalFile will do that
 ----------- office hours questions
 -- hw 5 - shuffle and main
 -- this, var case for eval??
 -- is parser error enough?
 -- any other errors???? (not unbound variables)

