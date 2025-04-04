{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Homework04 where

import Prelude(Show, Num(..), Eq(..), Ord(..), Char, Int, undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude(Maybe(..))
import Prelude((++), map, elem, filter)


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

import FiniteStatePart2

---------------------------------------
-- Setup for section 1

type SLG sy = ([sy], [sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy deriving (Eq, Show)

slg1 :: SLG SegmentCV
slg1 = ([C,V], [C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

---------------------------------------
-- Setup for section 2

data Either a b = First a | Second b deriving (Show,Eq)

re1 :: RegExp Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegExp Char
re2 = Star re1

re3 :: RegExp Int
re3 = Star (Concat ZeroRE (Lit 3))

re4 :: RegExp Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

isValidBigram::(Eq sy) => [(sy, sy)] -> [sy] -> [sy] -> Bool 
isValidBigram t f w =
              case w of
              [] -> False
              [x] ->  x `elem` f
              x:y:rest -> (x, y) `elem` t && isValidBigram t f (y:rest)

generatesSLG :: (Eq sy) => SLG sy -> [sy] -> Bool
generatesSLG (sigma, i, f, t) w =
             case w of
                  []-> False
                  (x:xs) -> x `elem` i && isValidBigram t f w

endsFromSLG :: SLG sy -> [ConstructedState sy]
endsFromSLG (_, _, ends, _) = map StateForSymbol ends

deltaFromSLG :: SLG sy -> [(ConstructedState sy, sy, ConstructedState sy)]
deltaFromSLG (_, starts, _, transitions) =
    map (\t -> let (s1, s2) = t in (StateForSymbol s1, s2, StateForSymbol s2)) transitions ++
    (map (\s -> (ExtraState, s, StateForSymbol s)) starts) 

slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA slg =
    let (symbols, starts, ends, transitions) = slg
        statesOfFSA = ExtraState : map (\x -> StateForSymbol x) symbols
    in (statesOfFSA, symbols, [ExtraState], endsFromSLG slg, deltaFromSLG slg)


unionFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
unionFSAs (states1, syms1, i1, f1, delta1) (states2, syms2, i2, f2, delta2) =
          let states = map First states1 ++ map Second states2 in
          let syms = nub (syms1 ++ syms2) in
          let i = map First i1 ++ map Second i2 in
          let f = map First f1 ++ map Second f2 in
          let delta = map (\(q, a, q') -> (First q, a, First q')) delta1
                    ++ map (\(q, a, q') -> (Second q, a, Second q')) delta2
          in (states, syms, i, f, delta)


concatFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
concatFSAs (states1, syms1, i1, f1, delta1) (states2, syms2, i2, f2, delta2) =
           let states = map First states1 ++ map Second states2 in
           let syms = nub (syms1 ++ syms2) in
           let i = map First i1 in
           let f = map Second f2 in
           let delta =  map (\(q, a, q') -> (First q, a, First q')) delta1
                     ++ map (\(q, a, q') -> (Second q, a, Second q')) delta2
                     ++ liftA2(\q1 q2 -> (First q1, Nothing, Second q2)) f1 i2
           in (states, syms, i, f, delta)

starFSA :: EpsAutomaton st sy -> EpsAutomaton (Either Int st) sy
starFSA (states, syms, i, f, delta) =
           let states' = First 0 : map Second states
               syms' = syms
               i' = [First 0]  
               f' = First 0 : map Second f 
               delta' = map (\(q, a, q') -> (Second q, a, Second q')) delta ++
                      map (\q -> (First 0, Nothing, Second q)) i ++
                      liftA2 (\q1 q2 -> (Second q1, Nothing, Second q2)) f i
               in (states', syms', i', f', delta')

flatten :: Either Int Int -> Int
flatten a = case a of First x -> 2 * x; Second y-> 2 * y + 1

mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates final (states, syms, i ,f, delta) =
          let states' = map final states in
          let i' = map final i in
          let f' = map final f in
          let delta' = map(\(q, a, q') -> (final q, a, final q')) delta
          in (states', syms, i', f', delta')


reToFSA :: (Eq sy) => RegExp sy -> EpsAutomaton Int sy
reToFSA x = case x of
        ZeroRE -> ([], [], [], [], []) 
        OneRE  -> ([0,1], [], [0], [1], [(0, Nothing, 1)])  
        Lit sy -> ([0,1], [sy], [0], [1], [(0, Just sy, 1)]) 
        Alt r1 r2 -> mapStates flatten ( unionFSAs (reToFSA r1) (reToFSA r2))
        Concat r1 r2 -> mapStates flatten (concatFSAs (reToFSA r1) (reToFSA r2))
        Star r -> mapStates flatten (starFSA (reToFSA r))
