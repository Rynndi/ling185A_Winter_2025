{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module TreeGrammars where

import Prelude(Show(..), Num(..), Eq(..), Ord(..), Char, undefined)
import Prelude(Bool(..), (&&), (||), not, or, and)
import Prelude(String, (++), map, filter, length, concat)

data BTree sy0 sy2 = BLeaf sy0 | BNode sy2 (BTree sy0 sy2) (BTree sy0 sy2) deriving Show

type Automaton st sy0 sy2 = ([st], [st], [(sy0,st)], [((st,st),sy2,st)])

--------------------------------------------------------------------------------
-- This is a modified version of the even/odd example from class, leaving 
-- out the 'f' nodes (since 'f' was a rank-one symbol there)

t15 :: BTree Char Char
t15 = BNode 'g' (BLeaf 'x') (BNode 'g' (BLeaf 'x') (BLeaf 'y'))

data Parity = Even | Odd deriving (Show,Eq)

fsta_even :: Automaton Parity Char Char
fsta_even = ([Even,Odd], [Even],
             [ ('x', Odd), 
               ('y', Even)
             ],
             [ ((Even,Even), 'g', Even), 
               ((Even,Odd),  'g', Odd), 
               ((Odd,Even),  'g', Odd), 
               ((Odd,Odd),   'g', Even)
             ]
            )

--------------------------------------------------------------------------------
-- Here's the NPI example from class

t20 :: BTree String ()
t20 =
    BNode () (
        BNode () (
            BLeaf "that"
        ) (
            BNode () (BLeaf "nobody") (BNode () (BLeaf "met") (BLeaf "anybody"))
        )
    ) (
        BNode () (BLeaf "surprised") (BLeaf "John")
    )

-- These three states are called "NEG", "LIC" and "0" respectively on the handout (sorry)
data NegStatus = Neg | LicNeg | NegOK deriving (Show,Eq)

fsta_npi :: Automaton NegStatus String ()
fsta_npi = let npis = ["anybody", "ever"] in
           let licensors = ["nobody", "not"] in
           let otherwords = ["that", "met", "surprised", "John"] in
           ([NegOK, LicNeg, Neg],
            [NegOK, LicNeg],
            map (\s -> (s, NegOK)) otherwords 
                ++ map (\s -> (s, LicNeg)) licensors 
                ++ map (\s -> (s, Neg)) npis,
            [((Neg,    Neg),    (), Neg), 
             ((NegOK,  Neg),    (), Neg), 
             ((Neg,    NegOK),  (), Neg), 
             ((NegOK,  NegOK),  (), NegOK), 
             ((LicNeg, Neg),    (), NegOK), 
             ((LicNeg, NegOK),  (), NegOK), 
             ((NegOK,  LicNeg), (), NegOK), 
             ((LicNeg, LicNeg), (), NegOK)
            ]
           )

