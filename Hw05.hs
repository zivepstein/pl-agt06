-- Homework 5.0: Monads
-- Due 2016-10-26
-- 
 {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}
-- 
-- In this homework, you'll be working with monads in a variety of ways:
-- to speed up a shuffling algorithm; to create a generic framework for
-- MapReduce; and to generate random values for testing.
-- 
-- In addition, you'll write a function that converts an AST representing
-- `do` notation to an AST representing `(>>=)` and `return`.
-- 
-- I've imported the libraries you'll need. *Look at them before you
-- start!* You'll want to use both the [Haskell language
-- documentation](https://www.haskell.org/documentation) as well as
-- [Hackage](https://hackage.haskell.org/) and
-- [Hoogle](https://www.haskell.org/hoogle/).
-- 
-- You are *of course* allowed to import other libraries. It may even make your solutions easier!
-- 
 module Hw05 where

 import Control.Monad
 import Data.List.Split
 import Data.Array.IO
 import Data.List
 import qualified Data.Map as Map
 import Data.Map (Map(..),(!))
 import System.Exit
 import System.Environment
 import System.Exit
 import System.IO
 import System.Random

 import Test.QuickCheck
-- 
-- 
-- **Problem (1): shuffling**
-- 
-- In this problem, we're going to "shuffle" a list, reordering it randomly.
-- 
-- To start, let's get familiar with
-- [`System.Random`](http://hackage.haskell.org/package/random-1.1/docs/System-Random.html#v:getStdRandom).
-- 
-- Write a function that takes two numbers, `low` and `high`, and returns
-- a random number `n` such that `low <= n <= high`, i.e., inclusively
-- within the range.
-- 
 rand :: Int -> Int -> IO Int
 rand low high = getStdRandom (randomR (low,high))
-- 
-- Now write a function that takes a list and shuffles it. The
-- straightforward algorithm is O(n<sup>2</sup>):
-- 
--   - Given a non-empty list `xs`,
--   - randomly split the list into an element `y` and the rest of the list `ys`,
--   - cons `y` onto the shuffling of `ys`.
-- 
-- Don't worry, we'll speed it up in a minute.
-- 
 slAux 0 (x:xs) = xs
 slAux n [] = []
 slAux n (x:xs) = x: slAux (n-1) xs
 shuffleList :: [a] -> IO [a]
 shuffleList [] = return []
 shuffleList l = 
				do i <- rand 0 (length l-1)
				   ((l!!i):) <$> shuffleList (slAux i l)
-- 
-- 
-- Don't forget that you can run `:set +s` to get timing information in
-- GHCi. My implementation on my computer runs `sum <$> shuffleList
-- [0..10000]` 3.26 seconds.
-- 
-- It turns out that there's a much faster, O(n) algorithm for shuffling:
-- the [Fisher--Yates
-- shuffle](https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle). It
-- works on arrays, not linked lists, so we'll have to use Haskell's
-- arrays.
-- 
-- Haskell's arrays are a little funny: arrays are parameterized by two
-- things: the type of their index and the monad in which they'll be
-- used. We'll work with
-- [`IOArray`s](http://hackage.haskell.org/package/array-0.5.1.0/docs/Data-Array-IO.html). The
-- `IOArray` type represents arrays that can be used in the `IO`
-- monad. We'll interact with these arrays using the [`MArray`
-- interface](http://hackage.haskell.org/package/array-0.5.1.0/docs/Data-Array-MArray.html).
-- 
-- Let's take a brief look at `IOArray`. It has kind `* -> * -> *`. The
-- first type it needs is the type of its indices... we can just use
-- `Int` for that, but it's interesting that we can use any type in the
-- `Ix` class. The second type it needs is the type of its
-- contents. Shuffling won't care about that, so we'll end up working
-- with `IOArray Int a`.
-- 
-- As a warmup, write a function that takes a list and generates a
-- corresponding array. It's worth noting that the bounds that Haskell
-- uses in, e.g.,
-- [`newListArray`](http://hackage.haskell.org/package/array-0.5.1.0/docs/Data-Array-MArray.html#v:newListArray)
-- are *inclusive*, per
-- [`Data.Ix`](http://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Ix.html).
-- 
 listToArray :: [a] -> IO (IOArray Int a)
 listToArray x = newListArray (0, (length x)-1) x
 lengthIO :: IOArray Int a -> IO Int
 lengthIO l = length <$> (getElems l)
-- 
-- 
-- Okay: let's do it. Implement the Fisher--Yates shuffling algorithm that takes a given
-- array and shuffles it.
-- 
 shuffleAux:: Int -> Int -> IOArray Int a -> IO ()
 shuffleAux n len arr = if n == len-2
			 	then return () 
				else 
 					do index <- rand n (len-1)
					   elem1 <- readArray arr n
					   elem2 <- readArray arr index
					   unused <- writeArray arr index elem1
					   unused2 <- writeArray arr n elem2
					   shuffleAux (n+1) len arr
-- 
 testShuffle a =
		do ioA <- listToArray a
		   Hw05.shuffle ioA  
-- 
 shuffle :: IOArray Int a -> IO ()
 shuffle arr = 
			do len <- lengthIO arr
			   shuffleAux 0 len arr				
-- 
-- 
-- Now use your array-based function `shuffle` to work on lists.
-- -- > toList arr n = 
-- -- >		 	do elt <- readArray arr n 
-- -- >			   elt : toList arr (n-1)
-- 
 toList:: IOArray Int a -> Int -> IO [a]
 toList arr 0 = return []
 toList arr n =
 					do elem1 <- readArray arr (n-1)
					   (elem1:) <$> toList arr (n-1)
-- 
-- 
 fastShuffle :: [a] -> IO [a]
 fastShuffle l =
			    do {ioA <- listToArray l;
             len <- lengthIO ioA;
             result <- Hw05.shuffle ioA; 
			       (toList ioA (len)) }
-- 
-- 
-- My version of `fastShuffle` runs much more quickly than the naive one:
-- `sum <$> fastShuffle [0..10000]` runs in 0.04 seconds!
-- 
-- Finally, write a function that reads in a file and shuffles its lines.
-- 
 shuffleFile :: FilePath -> IO String
 shuffleFile f = 
                do 
                    split <- (splitOn "\n") <$> (readFile f)
                    shuf <- fastShuffle split 
                    return $ intercalate "\n" shuf 
-- 
-- 
-- Finally, this shuffling program is useful enough that we should make
-- it a command. To do so, you'll need to write a `main` function, which
-- should have type `IO ()`.
-- 
-- First and foremost, to compile your program, you'll run a command
-- like: `ghc Hw05.lhs -main-is Hw05.main -o shuffle`. This will compile
-- your program to an executable called `shuffle`. (The `-main-is` flag
-- is necessary because the default in Haskell is to have a file named
-- Main.hs.)
-- 
-- How should your program behave? It should take a single, optional
-- argument indicating a filename to shuffle. Without an argument (or
-- with an argument of `-`), it should read from standard input. If more
-- than one argument is given, you should print a "usage" message on
-- standard error and exit with a non-zero exit code.
-- 
-- Either way, your program should read the lines of the input (file or
-- standard input), shuffle them, and then print out the shuffled input.
-- 
-- Look at the various `System` modules to find out how to parse
-- command-line arguments.
-- 
 main :: IO ()
 main = do
          args <- getArgs
          sf <- mainAux args
          putStrLn sf    

 mainAux :: [String] -> IO String
 mainAux [] = do 
                split <- (splitOn "\n") <$> getContents
                shuf <- fastShuffle split 
                return $ intercalate "\n" shuf 
 mainAux ("-":xs) = do 
                split <- (splitOn "\n") <$> getContents
                shuf <- fastShuffle split 
                return $ intercalate "\n" shuf 
 mainAux [arg] = shuffleFile arg
 mainAux _ = die "you wrong"

-- 
-- **Problem (2): monadic MapReduce**
-- 
-- [MapReduce](https://en.wikipedia.org/wiki/MapReduce) is a model for
-- data parallel computation. We'll look at data parallel programming
-- after Thanksgiving, but for now let's try to understand MapReduce as
-- is.
-- 
-- Our mappers will take an input of type `a` and produce a list of
-- key-value pairs, where keys have type `k` and values have type `v`.
-- 
 type Mapper a k v = a -> [(k,v)]
-- 
-- Our reducers take a key and list of values and produces a new (ideally
-- shorter!) list of values.
-- 
 type Reducer k v = k -> [v] -> [v]
-- 
-- The actual MapReduce implementaiton has three real phases: mapping,
-- shuffling, and reducing. Here's a simple implementation:
-- 
 mapReduce :: Ord k => Mapper a k v -> Reducer k v -> [a] -> [(k,[v])]
 mapReduce m r = reduce r . shuffleKeys . concatMap (map listifyVal . m)
   where listifyVal (k,v) = (k,[v])
         shuffleKeys = Map.fromListWith (++)
         reduce r = Map.toList . Map.mapWithKey r
-- 
-- The canonical MapReduce example is word count. (Riveting, isn't it?)
-- Here's how to implement word count in MapReduce: given a list of
-- documents, a mapper breaks a given document into its consituent words,
-- where appearance of a word maps to 1. After shuffling, identical words
-- will be grouped together; the reducer sums up each token.
-- 
 wordCount = mapReduce countWords sumCounts
  where countWords = map (\w -> (w,1)) . words
        sumCounts _ cs = [sum cs]
-- 
-- I'm not sure why Google is so proud of what amounts to eight lines of
-- code and a slow way to count words.
-- 
-- Let's modify the MapReduce paradigm to allow for monadic computations,
-- where our mappers and reducers are monadic computations, like so:
-- 
 type MapperM m a k v = a -> m [(k,v)]
 type ReducerM m k v = k -> [v] -> m [v]
-- 
-- Note that a `MapperM` returns its list of key-value pairs inside of
-- some monad `m`; `ReducerM` is similar.
-- 
-- Adapt `mapReduce` above to define `mapReduceM`:

 listToMonad :: Monad m => [m a] -> m [a]
 listToMonad [] = return []
 listToMonad (x:xs) =  (:) <$> x <*> listToMonad xs
 listifyVal (k,v) = (k,[v])
 shuffleThemKeyz :: Ord k => [(k,[v])] -> Map k [v]
 shuffleThemKeyz = Map.fromListWith (++)
 demonifyValues :: (Monad m) => (k, m[v]) -> m(k, [v])
 demonifyValues (k, a) = do{ a' <- a; return (k, a')}
 mapReduceM :: (Ord k, Monad m) => MapperM m a k v -> ReducerM m k v -> [a] -> m [(k,[v])]
 mapReduceM mp r input = do{ 
                            mappy <- listToMonad $  map mp input; 
                            natalie <- return $ shuffleThemKeyz $ map listifyVal (foldr (++) [] mappy);
                            listToMonad $ map demonifyValues $ Map.toList $ Map.mapWithKey r natalie
                            }
-- 
-- 
-- To test, here's an adaptation of the `wordCount` example above.
-- 
 wordCountM = mapReduceM countWords sumCounts
  where countWords = return . map (\w -> (w,1)) . words
        sumCounts w cs = do
          when (length cs > 1) $ putStrLn $ "Lots of " ++ w ++ "!"
          return [sum cs]
-- 
-- **Problem (3): QuickCheck**
-- 
-- We'll be using
-- [QuickCheck](http://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck.html)
-- to write some tests.
-- 
-- Write a QuickCheck property to check that `reverse` is *involutive*,
-- i.e., that reversing a reversed list yields the original list.
-- 
 prop_rev_involutive :: Eq a => [a] -> Bool
 prop_rev_involutive l = l == reverse (reverse l)
-- 
-- Write a QuickCheck property to check that checks the [Collatz
-- conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture) for a
-- given number greater than 0.
--
 isCollatz :: Int -> Bool
 isCollatz 1 = True
 isCollatz n = if n `mod` 2 == 0 then isCollatz $ n `div` 2 else isCollatz $ 3*n+1
 prop_Collatz n = n>0 ==> isCollatz n
-- 
-- 
 data ArithExp =
     Num Int
   | Plus ArithExp ArithExp
   | Times ArithExp ArithExp
   | Neg ArithExp
   deriving Show

 eval :: ArithExp -> Int
 eval (Num i) = i
 eval (Plus e1 e2) = eval e1 + eval e2
 eval (Times e1 e2) = eval e1 * eval e2
 eval (Neg e) = 0 - eval e
-- 
-- Write a generator that generates arbitrary `ArithExp`s. Use it to
-- define an `Arbitrary` instance for `ArithExp`... keep in mind that we
-- don't want to generate *giant* data structures, so you may need to keep track of sizes.
-- 
-- 
 instance Arbitrary ArithExp where
   arbitrary = sized arbitraryN 

 arbitraryN 0 = Num <$> (arbitrary::Gen Int)
 arbitraryN n = oneof [aNum n, aPlus n, aTimes n, aNeg n]

 aNum _ = Num <$> (arbitrary::Gen Int)
 aPlus n = Plus <$> (arbitraryN (n  `div` 2)) <*> (arbitraryN (n  `div` 2))
 aTimes n = Times <$> (arbitraryN (n  `div` 2)) <*> (arbitraryN (n  `div` 2))
 aNeg n = Neg <$> (arbitraryN (n  `div` 2))
-- 
-- Write a test to ensure that `Plus e e` behaves the same as `Times 2 e`
-- for all expressions `e`.
-- 
 prop_double = (\e -> eval (Plus e e) == eval (Times (Num 2) e))
