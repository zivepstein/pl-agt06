module Check where

import Syntax

import Data.Set (Set)
import qualified Data.Set as Set

fvExpr :: Expr -> Set VarName
fvExpr (Var x) = Set.singleton x
fvExpr (Lam x _ e) = x `Set.delete` fvExpr e
fvExpr (App e1 e2) = fvExpr e1 `Set.union` fvExpr e2
fvExpr Zero = Set.empty
fvExpr (Succ e) = fvExpr e

fvProgram :: Set VarName -> Program -> Set VarName
fvProgram _ [] = Set.empty
fvProgram seen (Let x e:prog) =
  (fvExpr e `Set.difference` seen) `Set.union` fvProgram (Set.insert x seen) prog
fvProgram seen (Run e:prog) =
  (fvExpr e `Set.difference` seen) `Set.union` fvProgram seen prog
