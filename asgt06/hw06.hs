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
 data Exp = Var Varname | App Exp Exp | Lambda Varname Exp | ZeroValue  | Succ Exp deriving (Eq, Show)

 atom, expr, app :: Parser Exp
 atom = (char '(' *> expr <* char ')') <|> Var <$> var 

 app =  App <$> ( atom <* spaceChar) <*> atom <|> atom
 expr = ws *> ((kw "lambda" *> lambdaParser ) <|> foldl1 App <$> (sepBy1 app spaceChar) <|> atom)
 -- <|> Lambda <$> kw "lambda" *> var  <* (satisfy == '.') <*> expr

 lambdaParser :: Parser Exp
 lambdaParser = ws *> (Lambda <$> ((var <* (kw ".")) <* ws) <*> expr <|> Lambda <$> (var) <*> lambdaParser)

 data Statement = Let Varname Exp | Expression Exp deriving (Show,Eq)
 type Program = [Statement] 

 letParser :: Parser Statement
 letParser = Let <$> (((kw "let" *> var) <* kw "=") <* ws) <*> expr 

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
 subst (ZeroValue) x e2 = ZeroValue
 subst (Succ expr) x e2 = Succ $ subst expr x e2

 type Store = Map Varname Exp

 eval:: Exp ->Store -> Exp
 eval (Lambda x e) s = Lambda x e  
 eval (App e1 e2) s = eval (subst e1' x e2') s where
                    (Lambda x e1') = eval (e1) s
                    (e2') = eval (e2) s
 eval (Var x) s = case Map.lookup x s of
                (Just y) -> eval y s
                Nothing -> error "unclosed expression" 

 evalStore:: Exp ->Store -> (Exp, Store)
 evalStore (Lambda x e) s = (Lambda x e, s)  
 evalStore (App e1 e2) s = evalStore (subst e1' x e2') s where
                    (Lambda x e1') = fst $ evalStore (e1) s
                    (e2') = fst $ evalStore (e2) s
 evalStore (Var x) s = case Map.lookup x s of
                (Just y) -> evalStore y s
                Nothing -> error "unclosed expression" 

 evalC:: Exp ->Store -> Exp
 evalC (Lambda x e) s = if ( (fv (Lambda x e) s) == Set.empty) 
                    then Lambda x e 
                    else error ("unbound variable " ++ (concat (Set.toAscList (fv (Lambda x e) s))))
                     
 evalC (App e1 e2) s =  evalC (subst e1' x e2') s 
                    where 
                      (Lambda x e1') = evalC (e1) s
                      (e2') = evalC (e2) s
 evalC (Var x) s = case Map.lookup x s of
                (Just y) -> evalC y s
                Nothing -> error "unclosed expression" 

 successor :: Exp
 successor = Lambda "n" (Succ (Var "n"))

 evalN:: Exp ->Store -> Exp
 evalN (Lambda x e) s = evalNAux (App (App (Lambda x e) successor) ZeroValue) s
 evalN (App e1 e2) s = evalN (subst e1' x e2') s where
                    (Lambda x e1') = evalN (e1) s
                    (e2') = evalN (e2) s
 evalN (Var x) s = case Map.lookup x s of
                (Just y) -> evalN y s
                Nothing -> error "unclosed expression" 
 evalN (Succ x) s = Succ (evalN x s)
 evalN ZeroValue s = Lambda "x" ZeroValue

 evalNAux:: Exp ->Store -> Exp
 evalNAux (Lambda x e) s = if ( (fv (Lambda x e) s) == Set.empty)    
                    then Lambda x e
                    else error ("unbound variable " ++ (concat (Set.toAscList (fv (Lambda x e) s))))
 evalNAux (App e1 e2) s = case evalNAux(e1) s of
                        (Lambda x e1') -> evalNAux(subst e1' x (evalNAux(e2) s)) s
                        (Succ x) -> Succ x
 evalNAux (Var x) s = case Map.lookup x s of
                (Just y) -> evalN y s
                Nothing -> error "unclosed expression" 
 evalNAux ZeroValue s = Lambda "x" ZeroValue
 evalNAux (Succ x) s = Succ (evalNAux x s)
------ unbound 2 needs a case for eval a var???????

 pevalC:: Program -> Store -> (Program,Store)
 pevalC [] s = ([],s)
 pevalC ((Let x e):xs) s = (fst pA, snd pA) where pA = pevalC xs (Map.insert x (evalC e s) s)
 pevalC (Expression e:xs) s = (Expression (evalC e s): (fst pA), snd pA) where pA = pevalC xs s


 peval:: Program -> Store -> (Program,Store)
 peval [] s = ([],s)
 peval ((Let x e):xs) s =  (fst pA, snd pA) where pA = peval xs (Map.insert x (eval e s) s)
 peval (Expression e:xs) s = (Expression (eval e s): (fst pA), snd pA) where pA = peval xs s


 fv :: Exp -> Store -> Set Varname
 fv (Var x) _ = Set.singleton x
 fv (Lambda x e) s = Set.difference (Set.difference (fv e s) (Set.singleton x)) (Map.keysSet s)
 fv (App e1 e2) s = Set.union (fv e1 s) (fv e2 s)
 fv (ZeroValue) s = Set.empty
 fv (Succ x) s = fv x s 




----------------------------------------------------------------------------
--Tests

 true', secnd, false' :: Exp
 true' = (Lambda "n" (Lambda "x" (Var "n")))
 false' = (Lambda "n" (Lambda "x" (Var "x")))
 --and' = (Lambda "x" (Lambda "y" (App(App((Var "x") (Var "y")) false'))))
 secnd = (Lambda "x" (Var "x"))
 omega = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))

 store = Map.fromList [("z", true'), ("s", secnd)]


 test1 = evalC (App (App true' secnd) omega) (Map.empty) == Lambda "x" (Var "x") 

 unbound = Lambda "x" (Var "y")

 unbound2 = App (Lambda "x" (Var "y"))(Lambda "x" (Var "z"))

 p1 = [Let "zero" (Lambda "s" (Lambda "z" (Var "z"))),Let "succ" (Lambda "n" (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))))),Expression (App (Var "succ") (App (Var "succ") (Var "zero"))),Expression (App (Var "succ") (App (Var "succ") (App (Var "succ") (Var "zero"))))]
 p1out = [Expression (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Lambda "s" (Lambda "z" (Var "z"))) (Var "s")) (Var "z"))))) (Var "s")) (Var "z"))))),Expression (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Lambda "s" (Lambda "z" (Var "z"))) (Var "s")) (Var "z"))))) (Var "s")) (Var "z"))))) (Var "s")) (Var "z")))))]

 p2 = [Let "zero" (Lambda "s" (Lambda "z" (Var "z"))),Let "succ" (Lambda "n" (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))))),Expression (App (Var "succ") (App (Var "succ") (Var "zero")))]

 pevalCTest1 = case (parse program "lambda s z. s ((lambda s z. s ((lambda s z. z) s z)) s z); lambda s z. s ((lambda s z. s ((lambda s z. s ((lambda s z. z) s z)) s z)) s z)") of 
              (Just x) -> fst x == fst(pevalC p1 Map.empty)
              Nothing -> error "whoops"

 pevalCTest2 = case (parse program "lambda s z. s ((lambda s z. s ((lambda s z. z) s z)) s z)") of 
              (Just x) -> fst x == fst(pevalC p2 Map.empty)
              Nothing -> error "whoops"

 true = Expression $ Lambda "n" ((Lambda "x" (Var "n"))) 
 false =  Expression $ Lambda "n" ((Lambda "x" (Var "x")))
 notBool =  Expression $ Lambda "b" (Lambda "x" (Lambda "y" (App (App (Var "b") (Var "x")) (Var "y"))))
 notFalse = Expression $ App false' (Lambda "b" (Lambda "x" (Lambda "y" (App (App (Var "b") (Var "x")) (Var "y")))))
 andBoolExp = (Lambda "x" (Lambda "y" (App(App (Var "x") (Var "y")) false')))
 trueAndFalseProg = Expression $ App (App andBoolExp (true')) false'
 booltest1 = pevalC [trueAndFalseProg] Map.empty == pevalC [false] Map.empty

 unboundTest1 = peval [Expression $ unbound] Map.empty
 unboundTest2 = pevalC [Expression $ unbound] Map.empty

 five = (Lambda "s" (Lambda "z" (App (Var "s") (App (Var "s") (App (Var "s") (App (Var "s") (App (Var "s") (Var "z"))))))));
 succExp = Lambda "n" (Lambda "s" (Lambda "z" (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))))
 plus = Expression (Lambda "m" (Lambda "n" (App (App (Var "m") (Var "succ")) (Var "n"))));

 six = App succExp five 
 
 main :: IO ()
 main = do
          args <- getArgs
          result <-  mainAux args
          if ("-n" `elem` args || "-cn" `elem` args || "-nc" `elem` args)
          then putStrLn ( concat $ prettyPrintSepN $ (map statementToExpr result))
          else putStrLn ( concat $ prettyPrintSep $ (map statementToExpr result))
          -- putStrLn mainAux args    


 mainAux :: [String] -> IO Program
 mainAux args = case argsHasFileName args of
        (Just filename) -> readAndEvalFile hasC hasN filename
        Nothing -> readfromStdInput hasC hasN 
        where 
          hasC = "-c" `elem` args || "-cn" `elem` args || "-nc" `elem` args 
          hasN = "-n" `elem` args || "-cn" `elem` args || "-nc" `elem` args 

 argsHasFileName :: [String] -> Maybe String
 argsHasFileName [] = Nothing
 argsHasFileName (x:xs) = if (head x == '-') && (length x > 1) then argsHasFileName xs else (Just x)

 readfromStdInput:: Bool -> Bool -> IO Program
 readfromStdInput hasC hasN =if hasN then  do{
                contents <- getContents;
                return $ parsedN (parse program contents) hasC}
                else do{
                contents <- getContents;
                return $ parsedC (parse program contents) hasC
              }


 parsedC :: Maybe (Program,String) -> Bool -> Program
 parsedC (Just x) isCFlag = if isCFlag then fst $ pevalC (fst x) Map.empty else fst $ peval (fst x) Map.empty
 parsedC Nothing isCFlag  = error "We cannot ever get here - so parse failed?"

 parsedN :: Maybe (Program,String) -> Bool -> Program
 parsedN (Just x) isCFlag = let (parsedExpr, store) = peval (fst x) Map.empty in
                   map ((\e -> Expression e) . (\e -> evalNAux (App (App (fst $ evalStore e store) successor) ZeroValue) (snd $ evalStore e store)) . statementToExpr) parsedExpr
 parsedN Nothing isCFlag  = error "We cannot ever get here - so parse failed?"

 readAndEvalFile ::Bool -> Bool -> FilePath -> IO Program
 readAndEvalFile hasC hasN f = if hasN then do{
                    contents <-  readFile f;
                    return $ parsedN (parse program contents) hasC}
                    else do{
                    contents <-  readFile f;
                    return $ parsedC (parse program contents) hasC}
 
 prettyPrintSep :: [Exp] -> [String]
 prettyPrintSep [] = []
 prettyPrintSep [x] = [prettyPrint [x] ]
 prettyPrintSep (x:xs) = (prettyPrint [x] ++ "\n") : prettyPrintSep xs 

 prettyPrintSepN :: [Exp] -> [String]
 prettyPrintSepN [] = []
 prettyPrintSepN [x] = [show (prettyPrintN x)]
 prettyPrintSepN (x:xs) = (show (prettyPrintN x) ++ "\n") : prettyPrintSepN xs   

 prettyPrint :: [Exp] -> String
 prettyPrint ((Lambda a x):ps) = "lambda " ++ a ++ ". ("++(prettyPrint [x]) ++") " ++ (prettyPrint ps)
 prettyPrint ((App e1 e2):ps) = "(" ++ prettyPrint [e1] ++ ") ("++ prettyPrint [e2]++ ")" ++ (prettyPrint ps) 
 prettyPrint ((Var x):ps) = x ++ (prettyPrint ps)
 prettyPrint [] = ""

 prettyPrintN :: Exp -> Int
 prettyPrintN (Succ x) = 1 + (prettyPrintN x) 
 prettyPrintN (ZeroValue) = 0 
 prettyPrintN (Lambda x e) = prettyPrintN e
 prettyPrintN (App x y) = error "Your program is not a number"
 prettyPrintN (Var x) = error "your program is not a number"

 statementToExpr :: Statement -> Exp 
 statementToExpr (Let x e) = e
 statementToExpr (Expression e) = e
--todo : church numerals + flags + write factorial  + fix pretty print (weird parenthesis, lambda double args, greek letter if time)
  --todo : church numerals, give some well needed love to prtty print and clean + test