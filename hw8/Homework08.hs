{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module Homework08 where

import Prelude(Show(..), Num(..), Eq(..), Ord(..), Char, Int, undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude(String, (++), map, filter, elem, length, concat)

import Control.Applicative(liftA, liftA2, liftA3)

import TreeGrammars


plainWords = ["John","Mary","ate","bought","knows","an","apple","books","yesterday","C","laughed","because"]
whWords = ["who","what","why"]
qWords = ["Q"]


------------------------------------------------------
-- Some tiny helpers for writing trees more compactly
-- (short for "leaf" and "node")

l :: a -> BTree a b
l x = BLeaf x

n :: BTree a () -> BTree a () -> BTree a ()
n t1 t2 = BNode () t1 t2

------------------------------------------------------

-- (1a)/(2a) `C John ate an apple yesterday'
tree1a :: BTree String ()
tree1a = n (l "C") (n (l "John") (n (n (l "ate") (n (l "an") (l "apple"))) (l "yesterday")))

-- (1b)/(2b) `Q John ate what yesterday'
tree1b :: BTree String ()
tree1b = n (l "Q") (n (l "John") (n (n (l "ate") (l "what")) (l "yesterday")))

-- (3a) `Q John ate an apple yesterday'
tree3a :: BTree String ()
tree3a = n (l "Q") (n (l "John") (n (n (l "ate") (n (l "an") (l "apple"))) (l "yesterday")))

-- (3b) `C John ate what yesterday'
tree3b :: BTree String ()
tree3b = n (l "C") (n (l "John") (n (n (l "ate") (l "what")) (l "yesterday")))

tree6 :: BTree String ()
tree6 = n (l "Q") (n (n (l "the") (l "girl")) (n (l "asked") tree1b))

tree14 =
    BNode False (l "Q") (BNode False (l "John") (BNode False (l "laughed")
        (BNode True (l "because") (BNode False (l "C") (BNode False (l "Mary") (BNode False (BNode False (l "bought") (l "books")) (l "why")))))
    ))


------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.


count:: (Eq b) => b -> BTree a b -> Int
count _ (BLeaf _) = 0
count x (BNode sym left right) =
      if x == sym then 1 + count x left + count x right
      else 0 + count x left + count x right

leftmostLeaf :: BTree a b -> a
leftmostLeaf (BLeaf val) = val
leftmostLeaf (BNode _ left _) = leftmostLeaf left

brackets :: BTree String a -> String
brackets (BLeaf val) = val
brackets (BNode _ left right) = "[" ++ brackets left ++ " " ++ brackets right ++ "]"


under :: (Eq st, Eq sy0, Eq sy2) => Automaton st sy0 sy2 -> BTree sy0 sy2 -> st -> Bool
under (st, fS, leafT, nodeT) (BLeaf x) q =
      elem (x, q) leafT

under (st, fS, leafT, nodeT) (BNode sy2 left right) q =
      canTrans nodeT
      where
      canTrans [] = False 
      canTrans (((q1, q2), sy2', q') : rest) =
               (sy2' == sy2 && q' == q && under (st, fS, leafT, nodeT) left q1 && under (st, fS, leafT, nodeT) right q2 || canTrans rest)

generates :: (Eq st, Eq sy0, Eq sy2) => Automaton st sy0 sy2 -> BTree sy0 sy2 -> Bool
generates (st, fS, leafT, nodeT) tree =
          canGenerates fS
          where
          canGenerates [] = False
          canGenerates (q: qs)=
                       under (st, fS, leafT, nodeT) tree q || canGenerates qs
                    


data State = OK | C | Q | WH | ADJ  deriving (Eq, Show)


fsta_wh1 :: Automaton State String ()
fsta_wh1 = ([OK, Q, WH, C], [OK], leafT, nodeT)
  where
   
    leafT =
        map (\word -> (word, WH)) whWords ++  
        map (\word -> (word, Q)) qWords ++   
        map (\word -> (word, C)) ["C"] ++   
        map (\word -> (word, OK)) (filter (\x -> not (x `elem` (whWords ++ qWords ++ ["C"]))) plainWords)


    nodeT =
      [ ((OK, OK), (), OK)
      ,((C, OK), (), OK)
      ,((OK, WH), (), WH)
      ,((WH, OK), (), WH)
      ,((Q, WH), (), OK)
      ,((C, WH), (), WH)
      ,((WH, Q), (), OK)
      ,((WH, C), (), WH)
      ]

adjunctWords = ["because", "that", "unless", "than" ]

fsta_wh2 :: Automaton State String Bool
fsta_wh2 = ([OK, Q, WH, C], [OK], leafT, nodeT)
  where
    leafT =
        map (\word -> (word, WH)) whWords ++  
        map (\word -> (word, Q)) qWords ++   
        map (\word -> (word, C)) ["C"] ++   
        map (\word -> (word, ADJ)) (filter (`elem` adjunctWords) plainWords) ++
        map (\word -> (word, OK)) (filter (\x -> not (x `elem` (whWords ++ qWords ++ ["C"] ++ adjunctWords))) plainWords)

    nodeT =
      [ ((OK, OK), (False), OK)
      ,((C, OK), (False), OK)
      ,((OK, WH), (False), WH)
      ,((WH, OK), (False), WH)
      ,((Q, WH), (False), OK)
      ,((C, WH), (False), WH)
      ,((WH, Q), (False), OK)
      ,((WH, C), (False), WH)

      ,((ADJ, WH), (True), ADJ)
      ,((OK, ADJ), (False), OK)
      ,((C, ADJ), (False), OK)
      ,((Q, ADJ), (True), OK)
      ,((Q, WH), (True), OK)
      ]


 