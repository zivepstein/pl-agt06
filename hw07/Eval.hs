module Eval where

import Syntax
import qualified Data.Map as Map
import Data.Map (Map)
data Error =
    UnboundVariable VarName
  | AppliedNonFunction Expr
  | SuccNonNat Expr
  | PoorlyTypedExpression Expr
  deriving Eq

type G = Map VarName Type

instance Show Error where
  show (UnboundVariable x) = "unbound variable " ++ x
  show (AppliedNonFunction e) = "sorry but i tried to apply non-function in " ++ show e
  show (SuccNonNat e) = "woefully tried to take successor of a non-natural (bad Church numeral?) in " ++ show e
  show (PoorlyTypedExpression e) = "very poorly typed expression " ++ show e ++ ". please try again!"

typeOf :: G -> Expr -> Type 
typeOf g (Var x) = case Map.lookup x g of
                (Just y) -> y
                Nothing -> error "untyped variable" 
typeOf g T = BoolT
typeOf g F = BoolT 
typeOf g (If e1 e2 e3) = case (typeOf g e1,typeOf g e2,typeOf g e3) of
                      (BoolT, t1,t2) -> if t1 == t2 then t2 else error "cases of if statement must match in type" 
                      (_,_,_) -> error "poorly typed if expression"
typeOf g e@(App e1 e2) = 
  case t1 of
    Func t11 t12 | t11 == t2 -> t12
    Func t11 t12 -> error "mismatch"
    _ -> error "expected function"
 where
  t1 = typeOf g e1
  t2 = typeOf g e2
typeOf g (Lam x t e) = Func t (typeOf (Map.insert x t g) e)
typeOf g (LetExp x e1 e2) = typeOf (Map.insert x (typeOf g e1) g) e2
typeOf g (Num x) = NumT
typeOf g (Pair e1 e2) = PairT (typeOf g e1) (typeOf g e2)
typeOf g p@(LetRec x t e1 e2) = if (typeOf (Map.insert x t g) e1) == t then typeOf (Map.insert x t g) e2 else error $ show (PoorlyTypedExpression p) 
-- typeOf g (AssignType e t) = if typeOf g e
typeOf g (UnopExp Not e) = if typeOf g e == BoolT then BoolT else error $ show (PoorlyTypedExpression (UnopExp Not e))
typeOf g (UnopExp Neg e) = if typeOf g e == NumT then NumT else error $ show (PoorlyTypedExpression (UnopExp Neg e))
typeOf g (UnopExp Fst (Pair e1 e2)) = typeOf g e1
typeOf g (UnopExp Snd (Pair e1 e2)) = typeOf g e2
typeOf g e@(BinopExp Plus e1 e2) = if typeOf g e1 == NumT && typeOf g e2 == NumT then NumT else error $ show (PoorlyTypedExpression e)
typeOf g e@(BinopExp Minus e1 e2) = if typeOf g e1 == NumT && typeOf g e2 == NumT then NumT else error $ show (PoorlyTypedExpression e)
typeOf g e@(BinopExp Times e1 e2) = if typeOf g e1 == NumT && typeOf g e2 == NumT then NumT else error $ show (PoorlyTypedExpression e)
typeOf g e@(BinopExp Div e1 e2) = if typeOf g e1 == NumT && typeOf g e2 == NumT then NumT else error $ show (PoorlyTypedExpression e)
typeOf g e@(BinopExp Equals e1 e2) = if typeOf g e1 == NumT && typeOf g e2 == NumT then NumT else 
        (if typeOf g e1 == BoolT && typeOf g e2 == BoolT then BoolT else error $ show (PoorlyTypedExpression e))
typeOf g e@(BinopExp And e1 e2) = if typeOf g e1 == BoolT && typeOf g e2 == BoolT then BoolT else error $ show (PoorlyTypedExpression e)
typeOf g e@(BinopExp Or e1 e2) = if typeOf g e1 == BoolT && typeOf g e2 == BoolT then BoolT else error $ show (PoorlyTypedExpression e)
typeOf g e@(AssignType e1 t) = if typeOf g e1 == t then t else error $ show (PoorlyTypedExpression e)

eval :: Expr -> Either Error Expr
eval (Var x) = Left $ UnboundVariable x
eval e@(Lam _ _ _) = Right $ e
eval e@(App e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    Lam x t eBody -> eval $ subst v2 x eBody
    _ -> Left $ AppliedNonFunction e
eval Zero = Right $ Zero
eval (Succ e) = do
  v <- eval e
  if isNat v
  then Right $ Succ v
  else Left $ SuccNonNat e
eval (LetExp var e1 e2) = eval $ App (Lam var (typeOf Map.empty e1) e2) (e1)
eval (If p e1 e2) = case eval p of
  Right ep -> if ep == T then eval e1 else if ep == F then eval e2 else Left $ PoorlyTypedExpression ep
  Left err -> Left err
eval e@(Pair e1 e2) = case (eval e1, eval e2) of
  (Right a, Right b) -> Right $ Pair a b
  _ -> Left $ PoorlyTypedExpression e
eval p@(AssignType e t) = if typeOf Map.empty e == t then eval e else Left $ PoorlyTypedExpression p
eval (Num x) = Right (Num x)
eval T = Right T
eval F = Right F
--eval (LetRec x e1 e2) = 
eval (UnopExp Not e) = case eval e of
                Right T -> Right F
                Right F -> Right T
                Right e' -> Left $ PoorlyTypedExpression e'
                Left e' -> Left e'
eval (UnopExp Neg e) = case eval e of
                Right (Num n) -> Right (Num (-n))
                Right e' -> Left $ PoorlyTypedExpression e'
                Left e' -> Left e'
eval p@(UnopExp Fst e) = case eval e of
                Right (Pair e1 _) -> Right e1
                _ -> Left $ PoorlyTypedExpression p
eval p@(UnopExp Snd e) = case eval e of
                Right (Pair _ e1) -> Right e1
                _ -> Left $ PoorlyTypedExpression p
eval (BinopExp Plus e1 e2) = case (eval e1, eval e2) of
                (Right (Num n), Right (Num i)) -> Right (Num (n +i))
                _ -> Left $ PoorlyTypedExpression (BinopExp Plus e1 e2)
eval (BinopExp Times e1 e2) = case (eval e1, eval e2) of
                (Right (Num n), Right (Num i)) -> Right (Num (n *i))
                _ -> Left $ PoorlyTypedExpression (BinopExp Times e1 e2)
eval (BinopExp Div e1 e2) = case (eval e1, eval e2) of
                (Right (Num n), Right (Num i)) -> Right (Num (n `div` i))
                _ -> Left $ PoorlyTypedExpression (BinopExp Div e1 e2)
eval (BinopExp Minus e1 e2) = case (eval e1, eval e2) of
                (Right (Num n), Right (Num i)) -> Right (Num (n - i))
                _ -> Left $ PoorlyTypedExpression (BinopExp Minus e1 e2)
eval (BinopExp And e1 e2) = case (eval e1, eval e2) of
                (Right T, Right F) -> Right F
                (Right T, Right T) -> Right T
                (Right F, Right F) -> Right F
                (Right F, Right T) -> Right F
                _ -> Left $ PoorlyTypedExpression (BinopExp And e1 e2)
eval (BinopExp Or e1 e2) = case (eval e1, eval e2) of
                (Right T, Right F) -> Right T
                (Right T, Right T) -> Right T
                (Right F, Right F) -> Right F
                (Right F, Right T) -> Right T
                _ -> Left $ PoorlyTypedExpression (BinopExp Or e1 e2) 
eval (BinopExp Equals e1 e2) = case eval e1 == eval e2 of
                True -> Right T
                False -> Right F



                



isNat :: Expr -> Bool
isNat Zero = True
isNat (Succ e) = isNat e
isNat _ = False

-- subst eX for x in e
subst :: Expr -> VarName -> Expr -> Expr
subst eX x (Var y) = if x == y then eX else Var y
subst eX x e@(Lam y t eBody) = if x == y then e else Lam y t $ subst eX x eBody
subst eX x (App e1 e2) = App (subst eX x e1) (subst eX x e2)
subst eX x Zero = Zero
subst eX x (Succ e) = Succ (subst eX x e)
subst eX x (Num i) = (Num i)
subst eX x T = T
subst eX x F = F
subst eX x (UnopExp u e) = UnopExp u (subst eX x e)
subst eX x (BinopExp u e1 e2) = BinopExp u (subst eX x e1) (subst eX x e2)
subst eX x (Pair e1 e2) = Pair (subst eX x e1) (subst eX x e2)
subst eX x (AssignType e1 t) = AssignType (subst eX x e1) t


substS :: Expr -> VarName -> Stmt -> Stmt
substS eX x s@(Let y e) = if x == y then s else Let y $ subst eX x e
substS eX x (Run e) = Run $ subst eX x e