import BoolExp
import qualified Data.Map as M

t = BVal True
f = BVal False

x = "x"
y = "y"

env1 = M.empty
env2 = M.insert x True env1
env3 = M.insert y False env2

type Test = (BEnv, BoolExp)

test1 = (env1, t) --t
test2 = (env1, f) --f
test3 = (env1, t :&&: (f :||: t) :&&: (Not f)) --t
test4 = (env2, BVar x) --t
test5 = (env3, BVar x :&&: BVar y) --f
test6 = (env3, BVar x :&&: (BVar y :||: (t :&&: (f :||: t)))) --t
test7 = (env3, BVar x :&&: (BVar y :||: (t :&&: (f :||: f)))) --f
test8 = (env3, Not (BVar y)) --t
test9 = (env3, Not (BVar y :&&: BVar x)) --t
test10 = (env3, BVar y :||: (BVar x :&&: BVar x)) --t

tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]

run_tests :: [Test] -> IO ()
run_tests ts = run_t 1 ts
  where
    run_t n []     = return ()
    run_t n ((env,b):ts) = do
      putStr ("test " ++ show n ++ ":\n")
      startBool env b
      putStr "\n"
      run_t (n+1) ts

main = run_tests tests