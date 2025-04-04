{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module ContextFree where

import Prelude(Show(..), Num(..), Eq(..), Ord(..), Char, Int, Double, Float, undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude(Maybe(..), lookup)
import Prelude(String, (++), map, filter, length, take, drop, concat)

-- You can ignore these imports, which are just used for printTable
import Prelude(IO, putStr, unlines, (.))

import Control.Applicative(liftA, liftA2, liftA3)

data Cat = S | NP | VP | PP | V | P deriving (Show, Eq)

data RewriteRule nt t = NTRule nt (nt,nt) | TRule nt t deriving (Show, Eq)

-- Corresponds to the definition in (3) on the handout
type CFG nt t = ([nt], [t], [nt], [RewriteRule nt t])

-----------------------------------------------------------
-- CFGs generalized to allow non-boolean result values

type GenericCFG nt t v = ([nt], [t], nt -> v, RewriteRule nt t -> v)

-- Feel free to ignore the details of this function.
makeGCFG :: (Eq nt, Eq t) => v -> ([nt], [t], [(nt,v)], [(RewriteRule nt t, v)]) -> GenericCFG nt t v
makeGCFG def (nts, ts, starts, rules) =
    let mylookup l x = case lookup x l of {Just y -> y; Nothing -> def} in
    (nts, ts, mylookup starts, mylookup rules)

-----------------------------------------------------------
-- Familiar semiring stuff, same as last week

class Semiring a where
    (&&&) :: a -> a -> a
    (|||) :: a -> a -> a
    gtrue :: a
    gfalse :: a

gen_or :: Semiring a => [a] -> a
gen_or list = case list of {[] -> gfalse; (x:xs) -> x ||| (gen_or xs)}

gen_and :: Semiring a => [a] -> a
gen_and list = case list of {[] -> gtrue; (x:xs) -> x &&& (gen_and xs)}

instance Semiring Bool where
    x &&& y = x && y
    x ||| y = x || y
    gtrue = True
    gfalse = False

instance Semiring Double where
    x &&& y = x * y
    x ||| y = x + y
    gtrue = 1.0
    gfalse = 0.0

-----------------------------------------------------------
-- Some example grammars

-- From (14) on the handout
cfg14 :: GenericCFG Cat String Bool
cfg14 = makeGCFG False ([VP,NP,PP,V,P], ["watches","spies","telescopes","with"], 
                        [(VP,True)], 
                        [(NTRule VP (V,NP), True),   (NTRule NP (NP,PP), True),     (NTRule PP (P,NP), True),
                         (NTRule VP (VP,PP), True),  (TRule NP "telescopes", True),
                         (TRule VP "watches", True), (TRule NP "watches", True),    (TRule P "with", True), 
                         (TRule VP "spies", True),   (TRule NP "spies", True),      (TRule V "watches", True)
                        ]
                       )

-- A probabilistic version of cfg14; this only assigns non-zero probabilities 
-- to things that are allowed by cfg14
cfg14a :: GenericCFG Cat String Double
cfg14a = makeGCFG 0.0 ([VP,NP,PP,V,P], ["watches","spies","telescopes","with"], 
                       [(VP,1.0)], 
                       [(NTRule VP (V,NP), 0.4),   (NTRule NP (NP,PP), 0.2),     (NTRule PP (P,NP), 1.0), 
                        (NTRule VP (VP,PP), 0.3),  (TRule NP "telescopes", 0.3), 
                        (TRule VP "watches", 0.2), (TRule NP "watches", 0.3),    (TRule P "with", 1.0), 
                        (TRule VP "spies", 0.1),   (TRule NP "spies", 0.2),      (TRule V "watches", 1.0)
                       ]
                      )

-- A probabilistic CFG that only ``noisily'' follows cfg14, where certain ``disallowed'' 
-- things can happen with some small probability
cfg14b :: GenericCFG Cat String Double
cfg14b = makeGCFG 0.0 ([VP,NP,PP,V,P], ["watches","spies","telescopes","with"], 
                       [(VP,0.9), (NP,0.025), (PP,0.025), (V,0.025), (P,0.025)], 
                       [(NTRule VP (V,NP), 0.38),  (NTRule NP (NP,PP), 0.2),      (NTRule PP (P,NP), 0.98), 
                        (NTRule VP (VP,PP), 0.3),  (TRule NP "telescopes", 0.29), (NTRule PP (NP,P), 0.01), 
                        (TRule VP "watches", 0.2), (TRule NP "watches", 0.29),    (TRule PP "with", 0.01), 
                        (TRule VP "spies", 0.1),   (TRule NP "spies", 0.2),       (TRule P "with", 1.0), 
                        (NTRule VP (V,PP), 0.01),  (TRule NP "with", 0.01),       (TRule V "watches", 1.0), 
                        (NTRule VP (NP,V), 0.01),  (NTRule NP (NP,NP), 0.01)
                       ]
                      )

-----------------------------------------------------------
-- Functions for inside values

insideBool :: GenericCFG nt t Bool -> [t] -> nt -> Bool
insideBool cfg str n =
    let (nts, ts, i, r) = cfg in
    case str of
    [] -> False
    (x:[]) -> r (TRule n x)
    (x:y:rest2) -> let conj i ld rd = r (NTRule n (ld,rd)) && 
                                      insideBool cfg (take i str) ld && 
                                      insideBool cfg (drop i str) rd
                   in
                   or (liftA3 conj [1 .. (length str - 1)] nts nts)

inside :: (Semiring v) => GenericCFG nt t v -> [t] -> nt -> v
inside cfg str n =
    let (nts, ts, i, r) = cfg in
    case str of
    [] -> gfalse
    (x:[]) -> r (TRule n x)
    (x:y:rest2) -> let conj i ld rd = r (NTRule n (ld,rd)) &&& 
                                      inside cfg (take i str) ld &&& 
                                      inside cfg (drop i str) rd
                   in
                   gen_or (liftA3 conj [1 .. (length str - 1)] nts nts)

------------------------------------------------------------------

-- This is the semiring-general version of (19) in this week's 
-- class handout.
f :: (Semiring v, Eq t, Eq nt) => GenericCFG nt t v -> [t] -> v
f g w =
    let (nts, ts, i, r) = g in
    gen_or (map (\nt -> i nt &&& fastInside g w nt) nts)

------------------------------------------------------------------
-- The simple inside function above ``works'', but can get 
-- very slow for large strings. A faster version, fastInside, 
-- is defined here that stores intermediate values in a table 
-- rather than recomputing them.

type Table k v = [(k,v)]

printTable :: (Show k, Show v, Semiring v, Eq v) => Table k v -> IO ()
printTable = putStr . unlines . map (\(k,v) -> show k ++ "\t" ++ show v) . filter (\(k,v) -> v /= gfalse)

findInTable :: (Eq k, Semiring v) => Table k v -> k -> v
findInTable assocs x = case lookup x assocs of {Just y -> y; Nothing -> gfalse}

fastInside :: (Semiring v, Eq nt, Eq t) => GenericCFG nt t v -> [t] -> nt -> v
fastInside g w n = findInTable (insideTable g w) (w,n)

insideTable :: (Semiring v, Eq nt, Eq t) => GenericCFG nt t v -> [t] -> Table ([t],nt) v
insideTable g w =
    let (nts, ts, i, r) = g in
    let m = length w in
    let cell len start nt = (take len (drop start w), nt) in
    let forLength l = liftA2 (cell l) [0 .. m-l] nts in
    let allCells = concat (map forLength [1..m]) in
    updateTable g [] allCells

updateTable :: (Semiring v, Eq nt, Eq t) => GenericCFG nt t v -> Table ([t],nt) v -> [([t],nt)] -> Table ([t],nt) v
updateTable g tbl [] = tbl
updateTable g tbl ((w,n):rest) =
    let newval = insideNonRec g (findInTable tbl) (w,n) in
    updateTable g (((w,n),newval):tbl) rest

insideNonRec :: (Semiring v) => GenericCFG nt t v -> (([t],nt) -> v) -> ([t],nt) -> v
insideNonRec g otherInside (w,n) =
    let (nts, ts, i, r) = g in
    case w of
    [] -> gfalse
    (x:[]) -> r (TRule n x)
    (x:y:rest2) -> let conj i ld rd = r (NTRule n (ld,rd)) &&& 
                                      otherInside (take i w, ld) &&& 
                                      otherInside (drop i w, rd)
                   in
                   gen_or (liftA3 conj [1 .. (length w - 1)] nts nts)

