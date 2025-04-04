{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module FiniteStatePart2 where

import Prelude(Show, Num(..), Eq(..), Ord(..), Char, Int, undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude(Maybe(..))
import Prelude((++), map, elem, filter, until, concat)

-- filter removes from a list elements that don't satisfy the given predicate
--      filter :: (a -> Bool) -> [a] -> [a]
--      e.g. filter (\x -> x > 3) [1,2,3,4,5]   ==>   [4,5]

-- liftA is equivalent to `map'; liftA2 and liftA3 generalize to functions of more arguments.
--      liftA :: (a -> b) -> [a] -> [b]
--      liftA2 :: (a -> b -> c) -> [a] -> [b] -> [c]
--      liftA3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
import Control.Applicative(liftA, liftA2, liftA3)

-- nub just removes duplicate elements from a list
--      nub :: (Eq a) => [a] -> [a]
--      e.g. nub [1,2,3,1,2,2,3,4,1]   ==>   [1,2,3,4]
import Data.List(nub)

----------------------------------------------------------------------------
-- Simple definitions

data SegmentCV = C | V deriving (Show,Eq)

type Automaton st sy = ([st], [sy], [st], [st], [(st,sy,st)])

-- FSA requiring an odd number of Cs
fsa_oddCs :: Automaton Bool SegmentCV
fsa_oddCs = ([True,False], [C,V], [False], [True], 
             [(False, C, True), (False, V, False), (True, C, False), (True, V, True)])

-- FSA requiring exactly two Vs
fsa_twoVs :: Automaton Int SegmentCV
fsa_twoVs = ([0,1,2], [C,V], [0], [2], [(0,C,0), (1,C,1), (2,C,2), (0,V,1), (1,V,2)])

----------------------------------------------------------------------------
-- Basic generation

backward :: (Eq st, Eq sy) => Automaton st sy -> [sy] -> st -> Bool
backward m w q =
    let (states, syms, i, f, delta) = m in
    case w of
    [] -> elem q f
    (x:rest) -> or (map (\q1 -> elem (q,x,q1) delta && backward m rest q1) states)

generates :: (Eq st, Eq sy) => Automaton st sy -> [sy] -> Bool
generates m w =
    let (states, syms, i, f, delta) = m in
    or (map (\q0 -> elem q0 i && backward m w q0) states)

----------------------------------------------------------------------------
-- Intersection of FSAs

intersect :: (Eq st, Eq st', Eq sy) => Automaton st sy -> Automaton st' sy -> Automaton (st,st') sy
intersect (states, syms, i, f, delta) (states', syms', i', f', delta') =
    let newStates = liftA2 (\x -> \y -> (x,y)) states states' in
    let newSyms = nub (syms ++ syms') in
    let newi = liftA2 (\x -> \y -> (x,y)) i i' in
    let newf = liftA2 (\x -> \y -> (x,y)) f f' in
    let candidateTransitions = liftA3 (\x -> \y -> \z -> (x,y,z)) newStates newSyms newStates in
    let isValidTransition ((q1,q1'), x, (q2,q2')) = elem (q1,x,q2) delta && elem (q1',x,q2') delta' in
    let newDelta = filter isValidTransition candidateTransitions in
    (newStates, newSyms, newi, newf, newDelta)

----------------------------------------------------------------------------
-- FSAs with epsilon transitions

-- Maybe types are pre-defined like this. You can think of them 
-- like a non-recursive list, with a maximum length of one.
--      data Maybe a = Nothing | Just a
--                     deriving (Eq,Show)

-- See (20) on the handout
type EpsAutomaton st sy = ([st], [sy], [st], [st], [(st, Maybe sy, st)])

-- See (21) on the handout
efsa21 :: EpsAutomaton Int Char
efsa21 = ([10,20,21,30,31,32], ['a','b'], 
                  [10], [20,30], [(10, Just 'a', 10), (10, Nothing, 20),  (10, Nothing, 30), 
                                  (20, Just 'b', 21), (21, Just 'b', 20), 
                                  (30, Just 'b', 31), (31, Just 'b', 32), (32, Just 'b', 30) ]
                 )

-- See (22) on the handout
efsa22 :: EpsAutomaton Int Char
efsa22 = ([0,1,2], ['a','b','c'], 
                  [0], [2], [(0, Just 'a', 0), 
                             (0, Nothing,  1), 
                             (1, Just 'b', 1), 
                             (1, Nothing,  2), 
                             (2, Just 'c', 2)]
                 )

-- One more epsilon-FSA
efsa_xyz :: EpsAutomaton Int Char
efsa_xyz = ([0,1], ['x','y','z'], [0], [1], [(0, Just 'x', 0), (0, Just 'y', 1), (0, Nothing, 1), (1, Just 'z', 1)])

-- Corresponds to (23) on the handout. Feel free to ignore the implementation of this one.
epsilonClosure :: (Eq st, Eq sy) => [(st, Maybe sy, st)] -> st -> [st]
epsilonClosure delta q =
    let outgoingEpsilons q' = filter (\(q1,x,q2) -> q1 == q' && x == Nothing) delta in
    let oneStepFrom q' = map (\(q1,x,q2) -> q2) (outgoingEpsilons q') in
    let update qs = nub (qs ++ (concat (map oneStepFrom qs))) in
    until (\qs -> update qs == qs) update [q]

-- See (24) on the handout.
removeEpsilons :: (Eq st, Eq sy) => EpsAutomaton st sy -> Automaton st sy
removeEpsilons (states, syms, i, f, delta) =
    let validTransition (q1,x,q2) = or (map (\q' -> elem q' (epsilonClosure delta q1) && elem (q', Just x, q2) delta) states) in
    let newDelta = filter validTransition (liftA3 (\x -> \y -> \z -> (x,y,z)) states syms states) in
    let canReachEnd q = or (map (\q' -> elem q' f) (epsilonClosure delta q)) in
    let newEnds = filter canReachEnd states in
    (states, syms, i, newEnds, newDelta)

----------------------------------------------

-- This follows the definition in (25) on the class handout.
data RegExp sy = Lit sy | Alt (RegExp sy) (RegExp sy) | Concat (RegExp sy) (RegExp sy) 
               | Star (RegExp sy) | ZeroRE | OneRE
               deriving Show

re26a, re26b, re26c :: RegExp Char
re26a = Alt (Lit 'a') (Lit 'b')
re26b = Concat re26a (Lit 'c')
re26c = Star re26b


