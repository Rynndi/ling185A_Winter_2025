{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module FiniteState where

import Prelude(Show, Eq, (==), Int, (+), (-), (*), (<), (<=), undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude((++), map, elem)

-- The imported function `(||)' works like this:
--      (||) -> Bool -> Bool -> Bool
--      x || y = case x of {True -> True; False -> y}

-- The imported function `or' works like this:
--      or :: [Bool] -> Bool
--      or l = case l of
--             []     -> False
--             x:rest -> x || (or rest)

-- The imported function `elem' works like this:
--      elem :: (Eq a) => a -> [a] -> Bool
--      elem y l = case l of
--                 []     -> False
--                 x:rest -> (x == y) || (elem y rest)

-- Fun facts:
--      or bools    ==   elem True bools
--      elem y xs   ==   or (map (\x -> x == y) xs)

-- A type we'll use for the symbols of some FSAs. 
-- By ``deriving Eq'' we are linking the default/obvious 
-- equality function on this type to the name (==).
data SegmentCV = C | V deriving (Show,Eq)

-- This defines the type `Automaton st sy' for all types st and sy. 
-- For example, `Automaton Int SegmentCV' is the type for automata that 
-- have states of type Int and symbols of type SegmentCV, and 
-- `Automaton Bool Char' is the type for automata that have states of 
-- type Bool and symbols of type Char, etc.
type Automaton st sy = ([st], [sy], [st], [st], [(st,sy,st)])

-- Here's the FSA from (4) on the handout
-- The type signature could equivalently have been given like this instead:
--       fsa4 :: ([Int], [SegmentCV], [Int], [Int], [(Int,SegmentCV,Int)])
fsa4 :: Automaton Int SegmentCV
fsa4 = ([40,41,42,43], [C,V], [40], [43], [(40, C, 40), (40, V, 40), 
                                           (40, C, 41), (40, V, 42), 
                                           (41, C, 43), (42, V, 43), 
                                           (43, C, 43), (43, V, 43)])

-- Here's the FSA from (5) on the handout
fsa5 :: Automaton Int SegmentCV
fsa5 = ([1,2,3], [C,V], [1], [3], [(1, C, 1), (1, V, 1), (1, V, 2), (2, C, 3)])

-- Here's the FSA from (6) on the handout
fsa6 :: Automaton Int SegmentCV
fsa6 = ([1,2,3], [C,V], [1], [1], [(1, V, 1), (1, C, 2), (1, V, 3), 
                                   (2, V, 1), (2, V, 3), 
                                   (3, C, 1)])

-- exactly follows (24) on the handout
backward :: (Eq st, Eq sy) => Automaton st sy -> [sy] -> st -> Bool
backward m w q = let (states, syms, i, f, delta) = m in
                 case w of
                 []       -> elem q f
                 (x:rest) -> or (map (\q1 -> elem (q,x,q1) delta && backward m rest q1) states)

-- exactly follows (21) on the handout
generates :: (Eq st, Eq sy) => Automaton st sy -> [sy] -> Bool
generates m w = let (states, syms, i, f, delta) = m in
                or (map (\q0 -> elem q0 i && backward m w q0) states)

