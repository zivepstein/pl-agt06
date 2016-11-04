Homework 1.0: Haskell warmup
Due 2016-09-07

Let's learn some Haskell! We'll be going over some rudiments in class,
and there's excellenft
[documentation](https://www.haskell.org/documentation) online.

In most places where I'd like you to fill in a definition, I've used
the convenient Haskell term `undefined`, which let's you compile
an incomplete program. (Running undefined parts of your program is an
error, and your program will crash.)

Please leave the following line in. (If you take it out, the grader
will reject your program.) We'll talk more about Haskell's module
system later in the semester.

> module Hw01 where

You can test this program by running `ghci` on it. If you edit
your code, you can use the `:reload` command to load in your new
definitions.

If your program has type errors, it won't compile. If your submitted
program doesn't compile, you will get no points.

The following imports are needed for Problem 9.

> import qualified Data.Map as Map
> import Data.Map (Map, (!))
>
> import qualified Data.Set as Set
> import Data.Set (Set)

**Problem 1: natural recursion**

Please don't use any Prelude functions to implement these---just write
natural recursion, like we did in class.

Write a function called `sumUp` that sums a list of numbers.

> sumUp :: [Int] -> Int
> sumUp []     = 0
> sumUp (x:xs) = x + sumUp xs

Write a function called `evens` that selects out the even numbers
from a list. For example, `evens [1,2,3,4,5]` should yield
`[2,4]`. You can use the library function `even`.

> evens :: [Int] -> [Int]
> evens []     = []
> evens (x:xs) 
>			| even x = x:evens xs
>			| otherwise = evens xs

Write a function called `incAll` that increments a list of numbers
by one. You'll have to fill in the arguments and write the cases yourself.

> incAll :: [Int] -> [Int]
> incAll [] = []
> incAll (x:xs) = x+1 : incAll xs

Now write a function called `incBy` that takes a number and
increments a list of numbers *by that number*.

> incBy :: Int -> [Int] -> [Int]
> incBy n [] = []
> incBy n (x:xs) =  x+n : incBy n xs

Write a function `append` that takes two lists and appends them.  For
example, `append [1,2] [3,4] == [1,2,3,4]`. (This function is called
`(++)` in the standard library... but don't use that to define your
version!)

> append :: [Int] -> [Int] -> [Int]
> append [] (y:ys) = y:ys
> append a [] = a
> append (x:xs) (y:ys) = x: append xs (y:ys)

**Problem 2: data types**

Haskell (and functional programming in general) is centered around
datatype definitions. Here's a definition for a simple tree:

> data IntTree = Empty | Node IntTree Int IntTree deriving (Eq,Show)

Write a function `isLeaf` that determines whether a given node is
a leaf, i.e., both its children are `Empty`.

> isLeaf :: IntTree -> Bool
> isLeaf Empty = False
> isLeaf (Node l x r) = l == Empty && r == Empty

Write a function `sumTree` that sums up all of the values in an
`IntTree`.

> sumTree :: IntTree -> Int
> sumTree Empty = 0
> sumTree (Node l x r) = x + sumTree l + sumTree r 

Write a function `fringe` that yields the fringe of the tree from
left to right, i.e., the list of values in the leaves of the tree,
reading left to right.

For example, the fringe of `Node (Node Empty 1 (Node Empty 2 Empty)) 5 (Node (Node Empty 7 Empty) 10 Empty)` is `[2,7]`.

> fringe :: IntTree -> [Int]
> fringe Empty = []
> fringe (Node l x r) 
>			| isLeaf (Node l x r)= [x]
>			| otherwise = append(fringe l) (fringe r)

**Problem 3: insertion sort**

Write a function `insertionSort` that takes a list of `Int`s
and produces one in sorted order. Use the [insertion sort
algorithm](https://en.wikipedia.org/wiki/Insertion_sort). You might
want to write a helper function.


> grow :: [Int] -> Int -> [Int]
> grow [] x = [x]
> grow (y:ys) x 
>			| x < y =  append [x] (y:ys)
>			| otherwise = append [y] (grow ys x)
> insertionSort :: [Int] -> [Int]
> insertionSort [] = []
> insertionSort (x:xs) =  insSortAux (x:xs) []
>		where 
>			insSortAux [] growList = growList
> 			insSortAux (x:xs) growList = insSortAux xs (grow growList x)

**Problem 4: binary search trees **

Write a function `isBST` to determine whether or not a given tree
is a strict binary search tree, i.e., the tree is either empty, or it
is node such that:

* all values in the left branch are less than the value of the node, and
* all values in the right branch are greater than the value of the node.

I've given you a helper function `maybeBounded` that checks whether a
given `Int` is bounded. It uses the Haskell `Maybe` type, which is
essentially defined as:

```haskell
data Maybe Int = Nothing | Just Int
```

`Maybe` makes a type *nullable*. In Java, every non-primitive type is
nullable---the `null` object can have any class. In Haskell, you must
explicitly ask for nullability, and nullness and non-nullness are
*both* explicit: `Nothing` is null, and the non-null `Just x`
holds a value `x`. We'll look at this more deeply in the next
assignment, when we talk about datatypes.

> maybeBounded :: Maybe Int -> Maybe Int -> Int -> Bool
> maybeBounded Nothing Nothing x = True
> maybeBounded Nothing (Just upper) x = x < upper
> maybeBounded (Just lower) Nothing x = lower < x
> maybeBounded (Just lower) (Just upper) x = lower < x && x < upper

> isBST :: IntTree -> Bool
> isBST Empty = True
> isBST (Node l x r) = (isBSTAux l x "left") && (isBSTAux r x "right")
>	where 
>		isBSTAux Empty y directon = True 
>		isBSTAux (Node l y r) x "left" = (maybeBounded Nothing (Just x) y ) && (isBSTAux l y "left") && (isBSTAux r y "right")
>		isBSTAux (Node l y r) x "right" =  (maybeBounded (Just x) Nothing y) && (isBSTAux l y "left") && (isBSTAux r y "right")
>

Write a function `insertBST` that performs BST insert. You may
assume your input is a BST.

> insertBST :: Int -> IntTree -> IntTree
> insertBST x Empty = (Node Empty x Empty)
> insertBST x (Node l y r) 
> 			| x < y = Node (insertBST x l) y r 
>			| otherwise = Node l y (insertBST x r)


Write a function `deleteBST` that removes a given value from a
BST. You may assume your input is a BST. Feel free to look up the
algorithm... I had to!

It doesn't really matter which algorithm you use, so long as the
function works correctly, i.e., for all BSTs `t`:

* `deleteBST x t` is a BST,
* `deleteBST x t` runs in O(log n) time in expectation,
* `x` doesn't appear in `deleteBST x t`,
* for all `y` in `t`, if `y /= x`, then `y` appears in `deleteBST y t`.

You are, as always, free to introduce any helper functions you might need.

> isInBST :: Int -> IntTree -> Bool
> isInBST x Empty = False
> isInBST x (Node l y r) = (x == y) || isInBST x l || isInBST x r
> 


> deleteBST :: Int -> IntTree -> IntTree
> deleteBST x Empty = Empty
> deleteBST y (Node l x r) = 
>		if isInBST y (Node l x r) 
>		then deleteBSTAux y (Node l x r) 
>		else (Node l x r)
> deleteBSTAux :: Int -> IntTree -> IntTree
> deleteBSTAux x Empty = Empty
> deleteBSTAux y (Node l x r) = undefined

**Problem 5: maps and folds**

We're going to define each of the functions we defined in Problem 1,
but we're going to do it using *higher-order functions* that are built
into [the
Prelude](http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html). In
particular, we're going to use `map`, `filter`, and the two folds,
`foldr` and `foldl`. To avoid name conflicts, we'll name all of the
new versions with a prime, `'`.

Define a function `sumUp'` that sums up a list of numbers.

> sumUp' :: [Int] -> Int
> sumUp' l = undefined

Define a function `evens'` that selects out the even numbers from a
list.

> evens' :: [Int] -> [Int]
> evens' l = undefined

Define a function `incAll'` that increments a list of numbers by
one.

> incAll' :: [Int] -> [Int]
> incAll' l = undefined

Define a function `incBy'` that takes a number and then increments
a list of numbers *by that number*.

> incBy' :: Int -> [Int] -> [Int]
> incBy' n l = undefined

Define a function `rev'` that reverses a list. Don't use
anything but a folding function (your choice), the list
constructors, and lambdas/higher-order functions.

> rev' :: [Int] -> [Int]
> rev' l = undefined

Define two versions of the function `append'` that appends two
lists.  One, `appendr`, should use `foldr`; the other,
`appendl`, should use `foldl`. You can use the list
constructors, higher-order functions, and `rev'`.

> appendr :: [Int] -> [Int] -> [Int]
> appendr l1 l2 = undefined
>
> appendl :: [Int] -> [Int] -> [Int]
> appendl l1 l2 = undefined

**Problem 6: defining higher-order functions**

We're going to define several versions of the `map` and `filter`
functions manually, using only natural recursion and folds---no using
the Prelude or list comprehensions Note that I've written the
*polymorphic* types for you.

Define `map1` using natural recursion.

> map1 :: (a -> b) -> [a] -> [b]
> map1 = undefined

Define `map2` using a folding function.

> map2 :: (a -> b) -> [a] -> [b]
> map2 f l = undefined

Define `filter1` using natural recursion.

> filter1 :: (a -> Bool) -> [a] -> [a]
> filter1 = undefined

Define `filter2` using a folding function.

> filter2 :: (a -> Bool) -> [a] -> [a]
> filter2 p l = undefined

**Problem 7: polymorphic datatypes **

We've already briefly seen the `Maybe` type in the first homework. In
the next two problems, we'll look at `Maybe`, pairs, and `Either` in
more detail.

Haskell's type system is rigid compared to most other languages. In
time, you will come to view this as a *feature*---languages that let
you 'cheat' their safety mechanisms end up making you pay for it with
complexity elsewhere. But for now, let's get familiar with the
structures and strictures of types.

The `Maybe` datatype introduces *nullability* in a controlled
fashion---values of the type `Maybe a` can be `Nothing` or `Just x`,
where `x` is a value of type `a`. Note that `Maybe` is polymorphpic:
we can choose whatever type we want for `a`, e.g., `Just 5 :: Maybe
Int`, or we can leave `a` abstract, e.g., `Just x :: Maybe a` iff `x ::
a`.

Write a function `mapMaybe` that behaves like `map` when its
higher-order function argument returns `Just x`, but filters out
results where the function returns `Nothing`.

> mapMaybe :: (a -> Maybe b) -> [a] -> [b]
> mapMaybe = undefined

The pair datatype allows us to aggregate values: values of type
`(a,b)` will have the form `(x,y)`, where `x` has type `a` and `y` has
type `b`.

Write a function `swap` that takes a pair of type `(a,b)` and returns
a pair of type `(b,a)`.

> swap :: (a,b) -> (b,a)
> swap = undefined

Write a function `pairUp` that takes two lists and returns a list of
paired elements. If the lists have different lengths, return a list of
the shorter length. (This is called `zip` in the prelude. Don't define
this function using `zip`!)

> pairUp :: [a] -> [b] -> [(a,b)]
> pairUp = undefined

Write a function `splitUp` that takes a list of pairs and returns a
pair of lists. (This is called `unzip` in the prelude. Don't define
this function using `unzip`!)

> splitUp :: [(a,b)] -> ([a],[b])
> splitUp = undefined

Write a function `sumAndLength` that simultaneously sums a list and
computes its length. You can define it using natural recursion or as a
fold, but---traverse the list only once!

> sumAndLength :: [Int] -> (Int,Int)
> sumAndLength l = undefined

**Problem 8: defining polymorphic datatypes**

The `Either` datatype introduces *choice* in a controlled
fashion---values of the type `Either a b` can be either `Left
x` (where `x` is an `a`) or `Right y` (where `y` is
a `b`).

Define a datatype `EitherList` that embeds the `Either` type into a
list. (This isn't a good idea, but it's a good exercise!)

To see what I mean, let's combine lists and the `Maybe`
datatype. Here's Haskell's list datatype:

```
data [a] = [] | a:[a]
```

Here's the Maybe datatype:

```
data Maybe a = Nothing | Just a
```

What kinds of values inhabit the type `[Maybe a]`? There are two cases:

- `[]`, the empty list
- `a:as`, where `a` has type `Maybe a` and `as` is a list of type `[Maybe a]`

But we can really split it into three cases:

- `[]`, the empty list
- `a:as`, where `as` is a list of type `[Maybe a]`, and:
  - `a` is `Nothing`
  - `a` is `Just a'`, where `a'` has type `a`

Put another way:

- `[]`, the empty list
- `Nothing:as`, where `as` is a list of type `[Maybe a]`
- `Just a:as`, where `a` has type `a` and `as` has type `[Maybe a]`

To define MaybeList, we'll write a data structure that has those
constructors expliclty.

```
data MaybeList a =
    Nil
  | ConsNothing (MaybeList a)
  | ConsJust a (MaybeList a)
```

Note that these match up exactly with the last itemized list of cases.

Okay: do it for `Either`! Fill in the functions below---they should
behave like the Prelude functions. You'll also have to fill in the
type. We've given you the constructors' names. Make sure your `Cons`
constructors takes arguments in the correct order, or we won't be able
to give you credit for *any* of this problem.

> data EitherList a b =
>     Nil
>   | ConsLeft {- fill in -}
>   | ConsRight {- fill in -}
>   deriving (Eq, Show)
>
> toEither :: [Either a b] -> EitherList a b
> toEither = undefined
>
> fromEither :: EitherList a b -> [Either a b]
> fromEither = undefined
>
> mapLeft :: (a -> c) -> EitherList a b -> EitherList c b
> mapLeft = undefined
>
> mapRight :: (b -> c) -> EitherList a b -> EitherList a c
> mapRight = undefined
>
> foldrEither :: (a -> c -> c) -> (b -> c -> c) -> c -> EitherList a b -> c
> foldrEither = undefined
>
> foldlEither :: (c -> a -> c) -> (c -> b -> c) -> c -> EitherList a b -> c
> foldlEither = undefined

**Problem 9: maps and sets**

Haskell has many convenient data structures in its standard
library. We'll be playing with sets and maps
today. [Data.Map](http://hackage.haskell.org/package/containers-0.5.6.3/docs/Data-Map-Lazy.html)
and
[Data.set](http://hackage.haskell.org/package/containers-0.5.6.3/docs/Data-Set.html)
are well documented on-line.

In this problem, we'll use maps and sets to reason about graphs (in
the network/graph theory sense, not in the statistical plotting sense).

We can start by defining what we mean by the nodes of the graph: we'll
have them just be strings. We can achieve this by using a *type
synonym*.

> type Node = String

To create a `Node`, we can use the constructor, like so:

> a = "a"
> b = "b"
> c = "c"
> d = "d"
> e = "e"

We can define a graph now as a map from `Node`s to sets of
`Node`s. The `Map` type takes two arguments: the type of the
map's *key* and the type of the map's *value*. Here the keys will be
`Node`s and the values will be sets of nodes. The `Set` type
takes just one argument, like lists: the type of the set's elements.

> type Graph = Map Node (Set Node)

We don't need to use `newtype` here, because we're less worried
about confusing graphs with other kinds of maps.

Let's start by building a simple graph, `g1`:

```pre
    - b -
   /     \
a -       - d
   \     /
    - c -
```

> g1 = Map.fromList [(a, Set.fromList [b,c]),
>                    (b, Set.fromList [a,d]),
>                    (c, Set.fromList [a,d]),
>                    (d, Set.fromList [b,c])]

Note that we've been careful to make sure the links are bidirectional:
if the `b` is in the value mapped by `a`, then `a` is in the value
mapped by `b`.

We can see what `a` has edges to by looking it up in `g1`:

> aEdges = g1 ! a

Write a function `isBidi` that checks whether a mapping is
bidirectional. Feel free to use any function in `Data.Map`,
`Data.Set`, or the Prelude, and write as many helper functions as you
need.

> isBidi :: Graph -> Bool
> isBidi = undefined

Write a function `bidify` that takes an arbitrary graph and makes it
bidirectional.

> bidify :: Graph -> Graph
> bidify = undefined

Be sure to test your code!
