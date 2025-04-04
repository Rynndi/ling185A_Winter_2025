{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Homework06 where


import Prelude(Show(..), Num(..), Eq(..), Ord(..), Char, Int, Double, Float, undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude(Maybe(..))
import Prelude(String, (++), elem, map, filter, length)

import Control.Applicative(liftA, liftA2, liftA3)

import ContextFree


----------------------------------------
-- Setup for section 1

data Tree nt t = NonLeaf nt (Tree nt t) (Tree nt t) | Leaf nt t deriving Show

tree1 :: Tree Cat String
tree1 = NonLeaf VP (NonLeaf VP (Leaf V "watches") (Leaf NP "spies"))
                   (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes"))

tree2 :: Tree Cat String
tree2 = NonLeaf VP (Leaf V "watches")
                   (NonLeaf NP (Leaf NP "spies") (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes")))

root :: Tree nt t -> nt
root tree = case tree of
            Leaf n x -> n
            NonLeaf n t1 t2 -> n

lhs :: RewriteRule nt t -> nt
lhs rule = case rule of
           TRule n x -> n
           NTRule n (l,r) -> n

----------------------------------------
-- Setup for section 2

type Automaton st sy = ([st], [sy], [st], [st], [(st,sy,st)])

data ConstructedNT st sy = NTforState st | NTforSymbol sy deriving (Show,Eq)

fsa1 :: Automaton Int Char
fsa1 = ([0,1], ['a','b','c'], [0], [1], [(0,'a',1), (0,'b',0), (0,'c',0), (1,'a',0), (1,'b',1)])

fsa2 :: Automaton Bool Int
fsa2 = ([False,True], [0,1], [False], [True], [(False,0,False), (False,0,True), (True,1,True)])

addBools :: (Eq nt, Eq t) => CFG nt t -> GenericCFG nt t Bool
addBools (states, syms, i, r) =
    makeGCFG False (states, syms, map (\q -> (q,True)) i, map (\t -> (t,True)) r)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

yield :: Tree nt t -> [t]
yield (Leaf _ t) = [t]
yield (NonLeaf _ l r) = yield l ++ yield r

treeToDeriv :: Tree nt t -> [RewriteRule nt t]
treeToDeriv (Leaf nt t) = [TRule nt t]
treeToDeriv (NonLeaf nt l r) = [NTRule nt (root l, root r)] ++ treeToDeriv l ++ treeToDeriv r


derivSteps :: (Eq nt) => ([t], [nt]) -> [RewriteRule nt t] -> Maybe ([t], [nt])
derivSteps state [] = Just state
derivSteps (ts, []) _ = Nothing
derivSteps (ts, nt:nts) (r:rules) =
    case (lhs r == nt) of
        True -> case r of
           TRule _ t -> derivSteps (ts ++ [t], nts) rules 
           NTRule _ (nt1, nt2) -> derivSteps (ts, nt1 : nt2 : nts) rules
        False -> Nothing

fsaToCFG :: (Eq st, Eq sy) => Automaton st sy -> CFG (ConstructedNT st sy) sy
fsaToCFG (states, syms, initial, final, transitions) =
    let nts = [NTforState q | q <- states] ++ [NTforSymbol s | s <- syms] in
    let ts = syms in
    let i = [NTforState q | q <- initial] in
    let rules = [(NTRule (NTforState q) (NTforSymbol s, NTforState q')) | (q, s, q') <- transitions] ++
                [(TRule (NTforSymbol s) s) | s <- syms] ++
               
                [(TRule (NTforState q) s) | (q, s, q') <- transitions, q' <- final]        

    in (nts, ts, i, rules)

evalTree :: (Semiring v) => GenericCFG nt t v -> Tree nt t -> v
evalTree cfg@(nts, ts, i, r) tree =
    let rootNT = root tree
        treeVal = evalTreeHelper cfg tree
    in i rootNT &&& treeVal 
  where
    evalTreeHelper (_, _, _, r) (Leaf nt t) = r (TRule nt t)
    evalTreeHelper cfg (NonLeaf nt left right) =
        let (nts, ts, i, r) = cfg
            leftRoot = root left
            rightRoot = root right
            ruleVal = r (NTRule nt (leftRoot, rightRoot))
            leftVal = evalTreeHelper cfg left
            rightVal = evalTreeHelper cfg right
        in ruleVal &&& leftVal &&& rightVal


probToProbList :: GenericCFG nt t Double -> GenericCFG nt t [Double]
probToProbList (nts, ts, i, r) =
    let convertInit nt = case i nt of
            0.0 -> []
            x -> [x]
        convertRule rule = case r rule of
            0.0 -> []
            x -> [x]
    in (nts, ts, convertInit, convertRule)

instance Semiring [Double] where
    xs &&& ys = [x * y | x <- xs, y <- ys] 
    xs ||| ys = xs ++ ys                   
    gtrue = [1.0]                           
    gfalse = []                            

boolToDerivList :: GenericCFG nt t Bool -> GenericCFG nt t [[RewriteRule nt t]]
boolToDerivList (nts, ts, i, r) =
    let convertInit nt = if i nt then [[]] else []
        convertRule rule = if r rule then [[rule]] else []
    in (nts, ts, convertInit, convertRule)

instance Semiring [[RewriteRule nt t]] where
    xs &&& ys = [x ++ y | x <- xs, y <- ys] 
    xs ||| ys = xs ++ ys                     
    gtrue = [[]]                             
    gfalse = []                               
