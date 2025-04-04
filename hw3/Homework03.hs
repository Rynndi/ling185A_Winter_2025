{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Homework03 where

import Prelude(Show, Eq, (==), Int, (+), (-), (*), (<), (<=), undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude((++), map, elem)

import FiniteState

-- Another type we'll use as symbols for some FSAs
data SegmentPKIU = P | K | I | U | MB deriving (Show,Eq)

-- A list-like type that will be useful for computing forward values
data SnocList a = ESL | (SnocList a) ::: a deriving Show

-- The string ``CVCCV'' encoded as a snoc list
sl :: SnocList SegmentCV
sl = ((((ESL ::: C) ::: V) ::: C) ::: C) ::: V

-- Checks that all states and symbols mentioned in the transition 
-- table (i.e. delta) come from the provided lists of states and symbols.
fsaSanityCheck :: (Eq st, Eq sy) => Automaton st sy -> Bool
fsaSanityCheck m =
    let (states, syms, i, f, delta) = m in
    let validTransition (q1,x,q2) = elem q1 states && elem x syms && elem q2 states in
    and (map validTransition delta)

-- The `deriving Eq' here means that the obvious/natural 
-- definition of equality will apply when we use (==) 
-- on these numbers.
data Numb = Z | S Numb deriving (Show,Eq)

one, two, three, four :: Numb
one = S Z
two = S one
three = S two
four = S three

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.



fsa_countTrues :: ( [Numb], [Bool], [Numb], [Numb], [(Numb, Bool, Numb)] )
fsa_countTrues = 
    ( [Z, S Z, S (S Z), S (S (S Z))], 
      [True, False],                   
      [Z],                               
      [S (S (S Z))],                     
      [ (Z, False, Z),
        (Z, True, S Z),
        (S Z, False, S Z),
        (S Z, True, S (S Z)),
        (S (S Z), False, S (S Z)),
        (S (S Z), True, Z),
        (S (S Z), True, S (S (S Z))),
        (S (S (S Z)), False, S (S (S Z)))
      ] 
    )

addToFront :: a -> SnocList a -> SnocList a
addToFront x a = case a of
           ESL -> ESL ::: x
           (rest ::: y) -> (addToFront x rest) ::: y


toSnoc :: [a] -> SnocList a
toSnoc a = buildSnoc ESL a
       where
        buildSnoc arr [] = arr
        buildSnoc arr (y:rest) = buildSnoc (arr ::: y) rest

forward :: (Eq st, Eq sy) => Automaton st sy -> SnocList sy -> st -> Bool
forward m w q = let (states, syms, i, f, delta) = m in
                case w of
                ESL -> elem q i
                ( rest ::: x ) -> or (map (\q1 -> forward m rest q1 && elem(q1, x, q) delta) states)

generates2 :: (Eq st, Eq sy) => Automaton st sy -> [sy] -> Bool
generates2 m w = let (states, syms, i, f, delta) = m
                     w' = toSnoc w
                 in or (map (\qf -> elem qf f && forward m w' qf) states)

fsa_twoCs:: Automaton Int SegmentCV
fsa_twoCs = ([0, 1, 2], [C, V], [0], [2],
            [(0, C, 1),
            (0, V, 0),
            (1, C, 2),
            (1, V, 1),
            (2, C, 2),
            (2, V, 2)
            ]
            )

fsa_thirdC :: Automaton Int SegmentCV
fsa_thirdC=  ([0, 1, 2, 3], [C, V], [0], [3],
            [
            (0, C, 1),
            (0, V, 1), 
            (1, C, 2),
            (1, V, 2),
            
            (2, C, 3), 
            (3, C, 3),
            (3, V, 3)
            ]
            )

fsa_thirdlastC :: Automaton Int SegmentCV
fsa_thirdlastC = ([0, 1, 2, 3], [C, V], [3], [0],
               [
               (1, C, 0),
               (1, V, 0),
               (2, C, 1),
               (2, V, 1),
               (3, C, 2),
            
               (3, C, 3),
               (3, V, 3)
               
               ]
               )

fsa_IU :: Automaton Int SegmentPKIU
fsa_IU = ([0, 1], [P, I, K, U], [0], [0,1],
         [
         (0, P, 0),
         (0, K, 0),
         (0, I, 1), 
        
         (1, P, 1),
         (1, K, 1),
         (1, I, 1),
         (1, U, 1)
         ]
         )


fsa_adjacentKU :: Automaton Int SegmentPKIU
fsa_adjacentKU = ([0, 1, 2], [P, I, K, U], [0], [0, 2],
                 [
                 (0, P, 0),
                 (0, K, 0),
                 (0, I, 0),
                 (0, K, 1),
                 (1, U, 2),
                 (2, P, 0),
                 (2, I, 0),
                 (2, K, 0),
                 (2, K, 1)
                 
                 ]
                 )


fsa_harmony :: Automaton Int SegmentPKIU
fsa_harmony = ([0, 1, 2], [P,K,I,U,MB], [0], [0,1,2],
              [
              (0, P, 0),
              (0, K, 0),
              (0, MB, 0),
              (0, I , 1),
              (1, MB, 0),
              (0, U, 2),
              (2, MB, 0),
              (0, MB, 2),
              (1, I, 1),
              (1, P, 1),
              (1, K, 1),
              (2, U, 2),
              (2, P, 2),
              (2, K, 2)
      
              ]
              )


fsa_oddEven :: Automaton Int SegmentCV
fsa_oddEven = ([0, 1, 2, 3], [C,V], [0], [2],
              [
              (0, V, 1),
              (1, V, 0),
              (1, C, 3),
              (3, C, 1),
              (0, C, 2),
              (2, C, 0),
              (2, V, 3),
              (3, V, 2)
             
              ]
              )

numbsLessThan :: Numb -> [Numb]
numbsLessThan a = case a of
              Z -> []
              S x -> x : numbsLessThan x

requireCs :: Numb -> Automaton Numb SegmentCV
requireCs n =
          let states = n : numbsLessThan n
              syms = [C, V]
              starts = [Z]
              ends = [n]
              ctransitions = map(\x -> (x, C, S x)) (numbsLessThan n)
              vtransitions = map(\x -> (x, V, x)) states

           in (states, syms, starts, ends, ctransitions ++ vtransitions)