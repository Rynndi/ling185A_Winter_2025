{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Homework05 where

import Prelude(Show, Num(..), Eq(..), Ord(..), Char, Int, Double, Float, undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude(Maybe(..))
import Prelude(max, maximum, product, sum)
import Prelude(String, (++), map)

import Control.Applicative(liftA, liftA2, liftA3)

import SemiringFSA

data Numb = Z | S Numb deriving Show

distrib_lhs :: (Semiring v) => v -> v -> v -> v
distrib_lhs x y z = x &&& (y ||| z)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

distrib_rhs :: (Semiring v) => v -> v -> v -> v
distrib_rhs x y z =  (y ||| z) &&& x

dotprod :: (Semiring v) => [v] -> [v] -> v
dotprod list1 list2 =
        case (list1, list2) of
             ([], _) -> gfalse
             (_, []) -> gfalse
             (x:x', y:y') -> (x &&& y) ||| dotprod x' y'

expn :: (Semiring v) => v -> Numb -> v
expn num pow =
        case pow of 
             Z -> gtrue
             S pow' -> num &&& expn num pow'

backward :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> st -> v
backward (states, syms, i, f, delta) w st =
         case w of
              [] -> f st
              (x:rest) -> gen_or [delta (st, x, st') &&& backward(states, syms, i, f, delta) rest st' | st' <- states]



f :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> v
f (states, syms, i, f, delta) w =
    gen_and [gen_or [i st &&& backward (states, syms, i, f, delta) w st | st <- states]]


addCost :: Cost -> Cost -> Cost
addCost a b =
        case (a, b) of
             (_, Inf) -> Inf
             (Inf, _) -> Inf
             (TheInt a, TheInt b) -> TheInt (a + b)

minCost :: Cost -> Cost -> Cost
minCost x y =
        case (x, y) of
             (TheInt a, TheInt b) -> if a < b then TheInt a else TheInt b
             (TheInt a, Inf) -> TheInt a
             (Inf, TheInt a) -> TheInt a
             (Inf, TheInt b) -> TheInt b
             (TheInt b, Inf) -> TheInt b
             (Inf, Inf)->Inf


instance Semiring Cost where
             x &&& y = addCost x y
             x ||| y = minCost x y
             gtrue = TheInt 0
             gfalse = Inf


instance Semiring [[a]] where
             x &&& y = [a ++ b | a <- x, b <- y]
             x ||| y = x ++ y
             gtrue = [[]]
             gfalse = []
             
gfsa39 :: GenericAutomaton Int Char [[Char]]
gfsa39 = makeGFSA [] ([1,2,3], ['C','V'],
                       [(1, [""])],  
                       [(1, [""])],  
                       [((1, 'C', 2), ["C"]),
                        ((1, 'V', 1), ["V"]),
                        ((1, 'V', 3), ["V"]),

                        ((2, 'V', 1), ["V", "VV"]),
                      
                        ((2, 'V', 3), ["V", "VV"]), 
                      

                        ((3, 'C', 1), [""])])  

gfsa_flap :: GenericAutomaton Int Char [[Char]]
gfsa_flap = makeGFSA [] ([0,1,2], ['a','n','t'],
                       [(0, [""])],  
                       [(0, [""]), (1, [""]),(2, ["t"])],  
                       [((0, 'n', 0), ["n"]),
                        ((0, 't', 0), ["t"]),
                        ((0, 'a', 1), ["a"]),

                        
                        ((1, 'a', 1), ["a"]),
                        ((1, 'n', 0), ["n"]),
                        ((1, 't', 2), [""]),
                        
                        

                        ((2, 'n', 0), ["tn"]),
                        ((2, 't', 0), ["tt"]),
                        ((2, 'a', 1), ["ta", "Ta"])
                     
                      

                     ])

gfsa7_count :: GenericAutomaton Int Char Double
gfsa7_count = makeGFSA 0.0 ([1,2,3], ['C','V'],
                     [(1, 1.0)],  
                     [(1, 1.0)],
                     [((1, 'V', 1), 1.0),
                     
                     ((1, 'C', 2), 1.0),
                     ((1, 'V', 3), 1.0),
                     
                     ((2, 'V', 1), 1.0),
                     ((2, 'V', 3), 1.0),
                     
                     ((3, 'C', 1), 1.0)])

gfsa7_paths :: GenericAutomaton Int Char [[Int]]
gfsa7_paths = makeGFSA [] ([1,2,3], ['C','V'],
                     [(1, [[1]])],  
                     [(1, [[]])],  
                     [((1, 'V', 1), [[1]]),  
                     ((1, 'C', 2), [[2]]),  
                     ((1, 'V', 3), [[3]]),  
                     ((2, 'V', 1), [[1]]),  
                     ((2, 'V', 3), [[3]]),  
                     ((3, 'C', 1), [[1]])])


boolToCount :: GenericAutomaton st sy Bool -> GenericAutomaton st sy Double
boolToCount (states, syms, i, f, delta) =
            let boolToDouble b = if b then 1.0 else 0.0
            in (states, syms, \st -> boolToDouble(i st), \st->boolToDouble (f st), \(q1, x, q2) -> boolToDouble (delta (q1, x, q2)))


boolToPaths :: GenericAutomaton st sy Bool -> GenericAutomaton st sy [[st]]
boolToPaths (states, syms, i, f, delta) =
            let validPath b st = if b then [[st]] else []  -- Convert `Bool` to `[[st]]`
                in ( states, syms, \st -> validPath (i st) st,\st -> if f st then [[]] else [],\(q1, x, q2) -> validPath (delta (q1, x, q2)) q2)

