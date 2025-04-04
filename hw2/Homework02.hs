module Homework02 where

-- Imports everything from the Recursion module. 
import Recursion

-- Imports just a few things that we have seen from the standard Prelude module. 
-- (If there is no explicit 'import Prelude' line, then the entire Prelude 
-- module is imported. I'm restricting things here to a very bare-bones system.)
import Prelude((+), (-), (*), (<), (>), (<=), (>=), Bool(..), Char, Int)

bigger :: Numb -> (Numb -> Numb)
bigger = \n -> (\m -> case n of
                      Z -> m
                      S n' -> case m of {Z -> n; S m' -> S ((bigger n') m')}
               )

numbToInt :: Numb -> Int
numbToInt = \n -> case n of
                  Z -> 0
                  S n' -> 1 + numbToInt n'

intToNumb :: Int -> Numb
intToNumb = \x -> case (x <= 0) of {True -> Z; False -> S (intToNumb (x-1))}

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.


mult :: Numb -> (Numb -> Numb)
mult = \a -> (\b -> case a of
                    Z -> Z
                    S a' -> add b (mult a' b)
                    )


sumUpTo:: Numb -> Numb
sumUpTo = \a -> case a of
        Z -> Z
        S x -> add a (sumUpTo x)
        

difference:: Numb -> (Numb -> Numb)
difference = \a -> (\b-> case (a, b) of 
             (Z, Z) -> Z
             (Z, x) -> x
             (x, Z) -> x
             (S a', S b') -> difference a' b'
                    )
count:: (a -> Bool) -> ([a] -> Numb)
count= \f -> \l ->  case l of
                [] -> Z
                x: rest -> case (f x) of
                           True -> S (count f rest)
                           False -> count f rest
                           
listOf:: Numb -> (a -> [a])
listOf = \a -> \b -> case a of
               Z -> []
               S a' -> b: listOf a' b

addToEnd:: a-> ([a] -> [a])
addToEnd = \a -> \b -> case b of
              [] -> [a]
              x:x' -> x: addToEnd a x'

remove :: (a -> Bool) -> ([a] -> [a])
remove= \f -> \l -> case l of
              [] -> [] 
              x:x' -> if f x
                      then remove f x'
                      else x : remove f x'

prefix :: Numb -> ([a] -> [a])
prefix= \a -> \b -> case a of {
        Z -> [];
        S a' -> case b of {[] -> []; x:x' -> x: (prefix a' x') }
        }

reverse :: [a] -> [a]
reverse= \a -> case a of
         [] -> []
         x:x' -> addToEnd x (reverse x')

countNegs :: Form -> Numb
countNegs = \a -> case a of
           T -> Z
           F -> Z
           Neg phi -> S (countNegs phi)
           Cnj phi psi -> add (countNegs phi) (countNegs psi)
           Dsj phi psi -> add (countNegs phi) (countNegs psi)

formDepth:: Form -> Numb
formDepth = \a -> case a of
          T -> S Z
          F -> S Z
          Neg left -> S (formDepth left)
          Cnj left right ->
              let leftDepth = formDepth left
                  rightDepth = formDepth right
              in S (bigger leftDepth rightDepth)
          Dsj left right ->
              let leftDepth = formDepth left
                  rightDepth = formDepth right
              in S (bigger leftDepth rightDepth)

         

          
          


           
           
           