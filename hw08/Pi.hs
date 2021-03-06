{-# LANGUAGE FlexibleInstances #-}

-- Implementation of the Syntax and Operational Semantics of the Pi Calculus

module Pi where

-- For documentation, see the following pages:
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Concurrent.html
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Concurrent-Chan.html

import Concurrent

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (concatMap)

-- Syntax of the Pi Calculus

type Name = String

instance Show (Chan Value) where
  show chan = "<channel>"

-- When reading through these data types, it is worth noting that *all* values
-- in this pi calculus are like locations in the STLC with references: they only
-- show up during evaluation, but *not* in programs a user might write.
--
-- In other words, the "abstract channel" object defined in your handout (as
-- "c" in the syntax) will actually be a Haskell channel (VChan below).  But
-- your translation will generate Pi terms, which only include expressions
-- (Exp), not values.

data Value
  = VChan (Chan Value)  -- channel value
  | VTup [Value]        -- tuple of values
  deriving Show

data Exp
  = EVar Name           -- variable expression
  | ETup [Exp]          -- tuple of expressions
  deriving Show

data Pattern
  = PVar Name           -- variable pattern
  | PTup [Pattern]      -- tuple pattern
  | Wild                -- wildcard pattern
  deriving Show

data Typ
  = TChan Typ           -- channel type
  | TTup [Typ]          -- tuple type
  deriving Eq

instance Show Typ where
  show (TChan t) = "Chan " ++ (show t)
  show (TTup []) = "()"
  show (TTup (h:ts)) = "(" ++ (show h) ++
    (concatMap (\x -> ", " ++ (show x)) ts) ++ ")"

instance Show (Env -> IO ()) where
  show f = "<function>"

data Pi
  = Nil
  | Pi :|: Pi
  | New Name Typ Pi
  | Out Name Exp
  | Inp Name Pattern Pi
  | RepInp Name Pattern Pi   -- repeated input
  | Embed (Env -> IO ()) Pi

instance Show Pi where
  show Nil = "0"
  show (p1 :|: p2) =
    "(" ++ (show p1) ++ ") | (" ++ (show p2) ++ ")"
  show (New x t p) =
    "new " ++ x ++ " : " ++ (show t) ++ ". " ++ (show p)
  show (Out x e) =
    "send " ++ x ++ "(" ++ (show e) ++ ")"
  show (Inp x pat p) =
    "rec " ++ x ++ "(" ++ (show pat) ++ "). " ++ (show p)
  show (RepInp x pat p) =
    "rec! " ++ x ++ "(" ++ (show pat) ++ "). " ++ (show p)
  show (Embed _ p) = "<function> " ++ (show p)

-- Useful Abbreviations

unitT :: Typ
unitT = TTup []

unitE :: Exp
unitE = ETup []

unitP :: Pattern
unitP = PTup []

printer :: String -> Pi
printer s = Embed (\_ -> putStr $ s ++ "\n") Nil

-- Static type checking

-- TASK!
-- Implement your pi calculus type checker here!

type Gamma = Map Name Typ

typeExp :: Gamma -> Exp -> Either String Typ
typeExp g (EVar n) = maybe2Either n g
typeExp g (ETup ls) = Right (TTup $ typeExpAux g ls)

typeExpAux :: Gamma -> [Exp] -> [Typ]
typeExpAux g [] = []
typeExpAux g (e:es) = case typeExp g e of
                  Right t -> t : (typeExpAux g es)
                  Left s -> error $ s++ " with with finding types in ttup: " ++ (show g) ++" trying to find " ++ (show e)

maybe2Either :: Name -> Gamma -> Either String Typ
maybe2Either n g = case Map.lookup n g of 
                    Nothing -> Left "nothing"
                    (Just t) -> Right t

typePat :: Gamma -> Pattern -> Typ -> Either String Gamma
typePat gamma (PTup []) (TTup []) = Right gamma
typePat gamma (PVar n) v = Right (Map.insert n v gamma)
typePat gamma Wild v = Right gamma
typePat gamma (PTup ((Wild):ns)) (TTup (t:ts)) = if length ns == length ts 
                                     then case (typePat gamma (PTup ns) (TTup ts)) of
                                      Right g -> Right $ Map.union gamma g
                                      Left s -> error "pattern typing failed for remaining list"
                                     else error "you fucked up"     
typePat gamma (PTup ((PVar n):ns)) (TTup (t:ts)) = if length ns == length ts 
                                     then case (typePat gamma (PTup ns) (TTup ts)) of
                                      Right g -> Right $ Map.union (Map.insert n t gamma) g
                                      Left s -> error "pattern typing failed for remaining list"
                                     else error "you fucked up"
typePat gamma p v = error $ "you did something wrong pattern and types are......." ++ (show p) ++ " " ++ (show v)                                     


checkPi :: Gamma -> Pi -> Either String ()
checkPi g Nil = Right ()
checkPi g (p1 :|: p2) = checkPi g p1 *> checkPi g p2
  -- case (checkPi g p1, checkPi g p2) of
  --                     (Right y, Right x )-> Right ()
  --                     (Right y, Left x) ->Left x
  --                     (Left y, _) -> Left y
checkPi g (New name t p) = checkPi (Map.insert name (TChan t) g) p
checkPi g (Out name e) = case (g ! name, typeExp g e) of
                    (TChan t1, Right t2) -> if t1 == t2 then Right () else Left ("type error for sending on a channel " ++ show t1 ++ " " ++ show t2 ++ " " ++ name)
                    -- (TTup t, Right (TTup t2)) -> if t == t2 then Right () else Left "type error for sending on a channel"
                    (TChan t1, Left e) -> Left e 
                    _ -> Left "merp"
checkPi g (Inp name pat p) = case (g!name) of
          TChan t -> do{g1 <- typePat g pat t; checkPi g1 p}
          _ -> error "trying to recieve on non channel type"
  -- case (typePat g pat (g! name), checkPi g p) of
  --                   (Right g1, Right ()) -> Right ()
  --                   (Right g1, Left e) -> Left e
  --                   (Left e, _ )-> Left e
checkPi g (RepInp name pat p) = case (g!name) of
          TChan t -> do{g1 <- typePat g pat t; checkPi g1 p }
          _ -> error "trying to recieve on non channel type"
  -- case (typePat g pat (g! name), checkPi g p) of
  --                   (Right g1, Right ()) -> Right ()
checkPi g (Embed f p) = checkPi g p
              


check :: Pi -> Either String ()
check p = checkPi Map.empty p

-- Signals a dynamic error

type_error :: String -> a
type_error s = error $ "Run-time Type Error: " ++ s

-- Environments for interpreters

-- TASK!
-- Implement your interpreter here!

type Env = Map Name Value

-- evalPat env p v
-- match a value v against a pattern p and extend environment env
evalPat :: Env -> Pattern -> Value -> Env
evalPat env (PVar []) (VTup []) = env
evalPat env (PVar n) v = (Map.insert n v env)
evalPat env Wild v = env
evalPat env (PTup ((PVar n):ns)) (VTup (v:vs)) = if length ns == length vs 
                                     then Map.union (Map.insert n v env) (evalPat env (PTup ns) (VTup vs)) 
                                     else error "you fucked up"
-- evalExp env e
-- evaluates e to a value in environment env
evalExp :: Env -> Exp -> Value
evalExp env (EVar x) = env ! x
evalExp env (ETup es) = VTup (evalExps env es)
  where
    evalExps env [] = []
    evalExps env (e:es) = evalExp env e : evalExps env es
--TODO: if we send more things to the channel than we read out no error is thrown / they're just ignored. Is this chill?
run :: Env -> Pi -> IO ()
run env Nil = pure ()
run env (p1 :|: p2) = parallel [run env p1, run env p2]
run env (New name t p) =  do{c <- newChan; run (Map.insert name (VChan c) env) p}
run env (Out name e) =  case  Map.lookup name env of 
                   (Just (VChan c)) -> writeChan c (evalExp env e)
                   (Just (VTup c)) -> error "can't send on multiple channels"
                   Nothing -> error ("no such channel env" ++ (show env) ++ " " ++ name)
run env (Inp name pat p) = do{
            r <- readChan (dvchan (env ! name));
            env' <- pure (evalPat env pat r) ;
            run env' p}
run env (RepInp name pat p) =  do 
            r <- readChan (dvchan (env ! name))
            let env' = (evalPat env pat r)
            parallel [run env' p, run env' (RepInp name pat p)]         
run env (Embed f p) = do{x <- f env; run env p}

send2front :: Pi -> Pi -> Pi
send2front p1 (p2a :|: p2b) =  (send2front p1 p2a) :|: p2b
send2front p1 x = p1 :|: x

subst :: Pi -> Name -> Name -> Pi
subst p n1 n2 = undefined

dvchan :: Value -> Chan Value
dvchan (VChan c) = c
dvchan (VTup ls) = error "yo cant send on multiple channels"

newName :: Env -> Name
newName env = if length (Map.keys env) == 0 then "1" else concat (Map.keys env) 

start :: Pi -> IO ()
start p = run Map.empty p

rev :: Pi -> Pi
rev (p1 :|: p2) = (p2 :|: rev p1)
rev x = x



-----For young natalie to ask young campbell:
      ---help with eval (:|: order of operations)
      		-- FAM WE WERE SUPPOSED TO USE parallel
      ---run(file="theory.txt", mentor=eric_campbell);
      		-- 	he's down with the theory part, just gotta maybe test at some point
      ---big picture booleans - how might we implement these??????????????

      ---new name business -> substituation, must we come up with a new name? does this make any sense? what is life?
      	-- na dog just don't need it and seems to work for peeps
      ---all the answers, plz