{-
Syntax and Implementation of Boolean Expressions
================================================
-}

module BoolExp where

import Pi
import qualified Data.Map.Strict as M

data BoolExp
  = BVar Name
  | BVal Bool
  | BoolExp :&&: BoolExp
  | BoolExp :||: BoolExp
  | Not BoolExp
  deriving Show

-- Environments for interpreting boolean expressions
type BEnv = M.Map Name Bool
-- TASK!
-- compileBExp tchan fchan b
-- returns a process p that when juxtaposed with a compatible environment
-- sends a message on tchan if the boolean expression evaluates to true
-- sends a message on fchan if the boolean expression evaluates to false
compileBExp :: Name -> Name -> BoolExp -> Pi
compileBExp tchan fchan (BVar n) = Inp (n++"fchan") Wild ((Out fchan (EVar " ")) :|: (Out (n++"fchan") (EVar " "))) 
                          :|: Inp (n++"tchan") Wild ((Out tchan (EVar " ")) :|: (Out (n++"tchan") (EVar " ")))
compileBExp tchan fchan (BVal b) = if b then (Out tchan (EVar " ")) else (Out fchan (EVar " "))
compileBExp tchan fchan (b1 :&&: b2) = 
                          New b1true unitT $ New b1false unitT $ 
                          New b2true unitT $ New b2false unitT $
                          compileBExp b1true b1false b1 
                          :|: compileBExp b2true b2false b2
                          :|: (Inp b1true Wild (Inp b2true Wild (Out tchan (EVar " "))))
                          :|: (Inp b1false Wild (Out fchan (EVar " ")))
                          :|: (Inp b2false Wild (Out fchan (EVar " ")))
                          where 
                            b1true = "b1"++tchan 
                            b1false = "b1"++fchan
                            b2true = "b2"++tchan 
                            b2false = "b2"++fchan
compileBExp tchan fchan (b1 :||: b2) =
                          New b1true unitT $ New b1false unitT $ 
                          New b2true unitT $ New b2false unitT $
                          compileBExp b1true b1false b1 
                          :|: compileBExp b2true b2false b2
                          :|: (Inp b1true Wild (Out tchan (EVar " ")))
                          :|: (Inp b2true Wild (Out tchan (EVar " ")))
                          :|: (Inp b1false Wild (Inp b2false Wild (Out fchan (EVar " "))))
                          where 
                            b1true = "b1"++tchan 
                            b1false = "b1"++fchan
                            b2true = "b2"++tchan 
                            b2false = "b2"++fchan
compileBExp tchan fchan (Not b) = New btrue unitT $ New bfalse unitT $ 
                          compileBExp btrue bfalse b 
                          :|: (Inp btrue Wild (Out fchan (EVar " ")))
                          :|: (Inp bfalse Wild (Out tchan (EVar " ")))
                          where
                            btrue = "b"++tchan
                            bfalse = "b"++fchan 
              

-- TASK!
-- compile a boolean variable environment into a process that
-- communicates with a compiled Boolean expression containing free
-- variables from the environment
-- for every element in the environemnt make the new channels and the out then run p in there 
compileBExpEnv :: BEnv -> Pi -> Pi
compileBExpEnv benv p = makeServers (M.toList benv) p

makeServers ::[(Name, Bool)] -> Pi -> Pi
makeServers [] p = p
makeServers [(n,b)] p = if b 
                  then New (n++"tchan") unitT (New (n++"fchan") unitT (sendUnit (n++"tchan") :|: p))
                  else New (n++"tchan") unitT (New (n++"fchan") unitT (sendUnit (n++"fchan") :|: p))
makeServers ((n,b):ls) p = if b 
                  then New (n++"tchan") unitT (New (n++"fchan") unitT (sendUnit (n++"tchan") :|: (makeServers ls p)))
                  else New (n++"tchan") unitT (New (n++"fchan") unitT (sendUnit (n++"fchan") :|: (makeServers ls p)))

sendUnit :: Name-> Pi
sendUnit c = Out c (EVar " ") 

testWithoutEnv :: BoolExp -> IO ()
testWithoutEnv bexp =  start pi
    where
      tchan = "t"
      fchan = "f"
      pi = New tchan unitT $
           New fchan unitT $
           (compileBExp tchan fchan bexp) :|:
           Inp tchan unitP (printer "true") :|:
           Inp fchan unitP (printer "false")

startBool :: BEnv -> BoolExp -> IO ()
startBool benv bexp = 
  start pi
    where
      tchan = "t"
      fchan = "f"
      pi = New tchan unitT $
           New fchan unitT $
           compileBExpEnv benv (compileBExp tchan fchan bexp) :|:
           Inp tchan unitP (printer "true") :|:
           Inp fchan unitP (printer "false")
