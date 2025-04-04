{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module SemiringFSA where

import Prelude(Show, Num(..), Eq(..), Ord(..), Char, Int, Double, Float, undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude(Maybe(..))
import Prelude(max, maximum, product, sum)
import Prelude(String, (++), map)

import Data.List(lookup)

-- Corresponds to (30) on the handout
type GenericAutomaton st sy v = ([st], [sy], st -> v, st -> v, (st,sy,st) -> v)

-- Feel free to ignore the details of this function.
makeGFSA :: (Eq st, Eq sy) => v -> ([st], [sy], [(st,v)], [(st,v)], [((st,sy,st),v)]) -> GenericAutomaton st sy v
makeGFSA def (states, syms, starts, ends, transitions) =
    let mylookup l x = case lookup x l of {Just y -> y; Nothing -> def} in
    (states, syms, mylookup starts, mylookup ends, mylookup transitions)

----------------------------------------------------------

-- Boolean-weighted FSA from (7) on the handout
gfsa7 :: GenericAutomaton Int Char Bool
gfsa7 = makeGFSA False ([1,2,3], ['C','V'],
                         [(1, True)], [(1, True)], 
                         [((1,'V',1), True),
                          ((1,'C',2), True),
                          ((1,'V',3), True),
                          ((2,'V',1), True),
                          ((2,'V',3), True),
                          ((3,'C',1), True)])

-- WFSA from (8) on the handout
gfsa8 :: GenericAutomaton Int Char Float
gfsa8 = makeGFSA 0 ([1,2,3], ['C','V'],
                    [(1, 1.0)], 
                    [(1, 1.0)], 
                    [((1,'V',1), 0.9),
                     ((1,'C',2), 1.0),
                     ((1,'V',3), 0.9),
                     ((2,'V',1), 1.0),
                     ((2,'V',3), 1.0),
                     ((3,'C',1), 0.8)])

-- PFSA from (25) on the handout
gfsa25 :: GenericAutomaton Int Char Double
gfsa25 = makeGFSA 0 ([1,2,3], ['C','V'],
                    [(1, 1.0)], 
                    [(1, 0.1)], 
                    [((1,'V',1), 0.2),
                     ((1,'C',2), 0.5),
                     ((1,'V',3), 0.2),
                     ((2,'V',1), 0.5),
                     ((2,'V',3), 0.5),
                     ((3,'C',1), 1.0)])

data Cost = TheInt Int | Inf deriving Show

-- Cost-weighted FSA from (37) on the handout
gfsa37 :: GenericAutomaton Int Char Cost
gfsa37 = makeGFSA Inf ([1,2,3], ['C','V'],
                       [(1, TheInt 0)], [(1, TheInt 0)], 
                       [((1,'V',1), TheInt 1),
                        ((1,'C',2), TheInt 0),
                        ((1,'V',3), TheInt 1),
                        ((2,'V',1), TheInt 0),
                        ((2,'V',3), TheInt 0),
                        ((3,'C',1), TheInt 2)])

----------------------------------------------------------
-- Setting up the semiring type class.

-- This says, ``Some types are semiring types. To be a semiring 
-- type you need to have two two-place operations called `&&&' and 
-- `|||', and two elements called `gtrue' and `gfalse'.''
class Semiring a where
    (&&&) :: a -> a -> a
    (|||) :: a -> a -> a
    gtrue :: a
    gfalse :: a

-- This says, ``Bool is a semiring type. When some code 
-- based on semirings uses `x &&& y', what that means for 
-- the type Bool is `x && y'; when some semiring code uses 
-- `x ||| y', what that means for Bool is `x || y'; etc.''
instance Semiring Bool where
    x &&& y = x && y
    x ||| y = x || y
    gtrue = True
    gfalse = False

-- Similarly for the Float type. 
-- When some code based on semirings uses `x &&& y', what 
-- that means for the type Float is `x * y'; etc.
instance Semiring Float where
    x &&& y = x * y
    x ||| y = max x y
    gtrue = 1.0
    gfalse = 0.0

-- Similarly for the Double type. 
-- When some code based on semirings uses `x &&& y', what 
-- that means for the type Double is `x * y'; etc.
instance Semiring Double where
    x &&& y = x * y
    x ||| y = x + y
    gtrue = 1.0
    gfalse = 0.0

----------------------------------------------------------
-- Now, some functions that will be usable with any semiring.

gen_or :: Semiring a => [a] -> a
gen_or list =
    case list of
    [] -> gfalse
    (x:xs) -> x ||| (gen_or xs)

gen_and :: Semiring a => [a] -> a
gen_and list =
    case list of
    [] -> gtrue
    (x:xs) -> x &&& (gen_and xs)

----------------------------------------------------------
-- FSA functions that only work with the Bool type

backwardBool :: GenericAutomaton st sy Bool -> [sy] -> st -> Bool
backwardBool m w q =
    let (states, syms, i, f, delta) = m in
    case w of
    [] -> f q
    (x:rest) -> or (map (\q1 -> delta (q,x,q1) && backwardBool m rest q1) states)

fBool :: GenericAutomaton st sy Bool -> [sy] -> Bool
fBool m w =
    let (states, syms, i, f, delta) = m in
    or (map (\q -> i q && backwardBool m w q) states)

----------------------------------------------------------
-- FSA functions that only work with the Float type (i.e. weights with max)

backwardFloat :: GenericAutomaton st sy Float -> [sy] -> st -> Float
backwardFloat m w q =
    let (states, syms, i, f, delta) = m in
    case w of
    [] -> f q
    (x:rest) -> maximum (map (\q1 -> delta (q,x,q1) * backwardFloat m rest q1) states)

fFloat :: GenericAutomaton st sy Float -> [sy] -> Float
fFloat m w =
    let (states, syms, i, f, delta) = m in
    maximum (map (\q -> i q * backwardFloat m w q) states)

