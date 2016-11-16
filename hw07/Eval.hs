module Eval where

import Syntax

data Error =
    UnboundVariable VarName
  | AppliedNonFunction Expr
  | SuccNonNat Expr
  deriving Eq

instance Show Error where
  show (UnboundVariable x) = "unbound variable " ++ x
  show (AppliedNonFunction e) = "tried to apply non-function in " ++ show e
  show (SuccNonNat e) = "tried to take successor of a non-natural (bad Church numeral?) in " ++ show e

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

substS :: Expr -> VarName -> Stmt -> Stmt
substS eX x s@(Let y e) = if x == y then s else Let y $ subst eX x e
substS eX x (Run e) = Run $ subst eX x e
