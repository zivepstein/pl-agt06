Homework 2.0: Type classes
Due 2016-09-14

> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

In this homework, we're going to use Haskell more earnestly. We'll
start using some of its standard library's functions and
datatypes---we'll even try defining our own datatypes.

Unless I say otherwise, you're free to use any functions from [the
Prelude](http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html).

> module Hw02 where

**Problem 1: arithmetic expressions**

Our first language will be a simple one: arithmetic expressions using
+, *, and negation.

> data ArithExp =
>     Num Int
>   | Plus ArithExp ArithExp
>   | Times ArithExp ArithExp
>   | Neg ArithExp

*(a) 5 points*

Write a `Show` instance for `ArithExp`, which amounts to a function
`show :: ArithExp -> String`. You do not need to write type signatures
for functions in type class instances.

Your function should print a parseable expression, i.e., one that you
could copy and paste back in to Haskell to regenerate the original
term. For example, `show (Num 5)` should yield the string `"Num 5"`,
while `show (Neg (Plus (Num 1) (Num 1)))` should yield the string
`"Neg (Plus (Num 1) (Num 1))"`.

> instance Show ArithExp where
>   show = undefined

*(b) 5 points*

Write an `Eq` instance for `ArithExp`, defining a function `(==) ::
ArithExp -> ArithExp -> Bool`.

> instance Eq ArithExp where
>   e1 == e2 = undefined

*(c) 10 points*

We're going to write an *interpreter*, which takes an arithmetic
expression and evaluates it to a number. The general strategy here is
the same as when we wrote naturally recursive functions over lists:
break down each case of the datatype definition and use recursion on
subparts.

For example, `eval (Plus (Num 42) (Neg (Num 42)))` should yield `0`.

> eval :: ArithExp -> Int
> eval = undefined

*(d) 10 points*

Let's extend our language to support subtraction---now we're really
cooking! Note that we let Haskell derive a "parseable" show instance
for us.

> data ArithExp' =
>     Num' Int
>   | Plus' ArithExp' ArithExp'
>   | Sub' ArithExp' ArithExp'
>   | Times' ArithExp' ArithExp'
>   | Neg' ArithExp'
>   deriving Show

But wait: we should be able to *encode* subtraction using what we
have, giving us a very nice evaluation function.

> eval' :: ArithExp' -> Int
> eval' = eval . translate

Write a function that will translate this extended language to our
original language---make sure that `eval'` does the right thing.

> translate :: ArithExp' -> ArithExp
> translate = undefined

*(e) 5 points*

Write a non-standard `Eq` instance for `ArithExp'`, where `e1 == e2`
iff they evaluate to the same number, e.g., `(Num 2) == (Plus (Num 1)
(Num 1))` should return `True.

> instance Eq ArithExp' where
>   e1 == e2 = undefined

Write a non-standard `Ord` instance for `ArithExp` that goes with the
`Eq` instance, i.e., `e1 < e2` iff `e1` evaluates to a lower number
than `e2`, etc.

> instance Ord ArithExp' where
>   compare e1 e2 = undefined

**Problem 2: `Setlike` (10pts)**

Here is a type class `Setlike`. A given type constructor `f`, of kind
`* -> *`, is `Setlike` if we can implement the following methods for
it. (Recall `Listlike` from [lecture](/lec/lec17.html).)

> class Setlike f where
>   emp :: f a
>
>   singleton :: a -> f a
>
>   union :: Ord a => f a -> f a -> f a
>   union = fold insert
>
>   insert :: Ord a => a -> f a -> f a
>   insert = union . singleton
>
>   delete :: Ord a => a -> f a -> f a
>   delete x s = fold (\y s' -> if x == y then s' else insert y s') emp s
>
>   isEmpty :: f a -> Bool
>   isEmpty = (==0) . size
>
>   size :: f a -> Int
>   size = fold (\_ count -> count + 1) 0
>
>   isIn :: Ord a => a -> f a -> Bool
>   isIn x s = maybe False (const True) $ getElem x s
>
>   getElem :: Ord a => a -> f a -> Maybe a
>
>   fold :: (a -> b -> b) -> b -> f a -> b
>
>   toAscList :: f a -> [a] -- must return the list sorted ascending
>   toAscList = fold (:) []

In the rest of this problem, you'll define some instances for
`Setlike` and write some code using the `Setlike` interface. Please
write the best code you can. `Setlike` has some default definitions,
but sometimes you can write a function that's more efficient than the
default. *Do it.* Write good code.

Define an instance of `Setlike` for lists. Here's an example that should work when you're done---it should be the set {0,2,4,6,8}.

> evensUpToTen :: [Int]
> evensUpToTen = fold insert emp [0,2,4,6,8]


Here's a type of binary trees. Define a `Setlike` for BSTs, using
binary search algorithms. *Write good code.* I expect insertion,
lookup, and deletion to all be O(log n).

> data BST a = Empty | Node (BST a) a (BST a)


Write `Eq` and `Show` instances for BSTs. These might be easier to write using the functions below.

> instance Ord a => Eq (BST a) where
>   s1 == s2 = undefined

> instance Show a => Show (BST a) where
>   show = undefined

Write the following set functions. You'll have to use the `Setlike`
interface, since you won't know which implementation you get.

`fromList` should convert a list to a set.

> fromList :: (Setlike f, Ord a) => [a] -> f a
> fromList = undefined

`difference` should compute the set difference: X - Y = { x in X | x not in Y }.

> difference :: (Setlike f, Ord a) => f a -> f a -> f a
> difference xs ys = undefined

`subset` should determine whether the first set is a subset of the
other one. X &sube; Y iff &forall; x. x &isin; X implies x &isin; Y.

> subset :: (Setlike f, Ord a) => f a -> f a -> Bool
> subset xs ys = undefined

**Problem 3: maps from sets (10pts)**

Finally, let's use sets to define maps---a classic data structure approach.

We'll define a special notion of key-value pairs, `KV k v`, with
instances to force comparisons just on the key part.

> newtype KV k v = KV { kv :: (k,v) }
>
> instance Eq k => Eq (KV k v) where
>   (KV kv1) == (KV kv2) = fst kv1 == fst kv2
>
> instance Ord k => Ord (KV k v) where
>   compare (KV kv1) (KV kv2) = compare (fst kv1) (fst kv2)
>
> instance (Show k, Show v) => Show (KV k v) where
>   show (KV (k,v)) = show k ++ " |-> " ++ show v

> type Map f k v = f (KV k v)
> type ListMap k v = Map [] k v
> type TreeMap k v = Map BST k v

Now define the following map functions that work with `Setlike`.

> emptyMap :: Setlike f => Map f k v
> emptyMap = undefined
>
> lookup :: (Setlike f, Ord k) => k -> Map f k v -> Maybe v
> lookup k m = undefined
>
> extend :: (Setlike f, Ord k) => k -> v -> Map f k v -> Map f k v
> extend k v m = undefined
>
> remove :: (Setlike f, Ord k) => k -> Map f k v -> Map f k v
> remove k m = undefined
>
> toAssocList :: Setlike f => Map f k v -> [(k,v)]
> toAssocList = undefined

You'll have to think hard about what to do for `lookup` and
`remove`... what should `v` be?

**Problem 4: functors (15pts)**

*(a) 5pts*

The n-ary tree, trie, or rose tree data structure is a tree with an
arbitrary number of children at each node. We can define it simply in
Haskell:

> data RoseTree a = Leaf a | Branch [RoseTree a] deriving (Eq, Show)

Define a `Functor` instance for `RoseTree`.

> instance Functor RoseTree where
>   fmap = undefined

*(b) 10pts*

Define a `Functor` instance for `BST`.

Give an example of a buggy behavior for your instance: this can either
be a violation of the `Functor` laws, or something else. Explain what
the issue is.