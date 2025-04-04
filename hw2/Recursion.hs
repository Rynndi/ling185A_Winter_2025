module Recursion where

-- By default, the entire Prelude module is always loaded, which provides 
-- a whole bunch of standard stuff. This explicit import statement causes 
-- only the specified things to be imported.
import Prelude(Bool(..), Show, Int, (+), (-), (*), (<), (<=))

data Form = T | F | Neg Form | Cnj Form Form | Dsj Form Form deriving Show

f1 = Dsj (Neg T) (Cnj F T)

removeNegs :: Form -> Form
removeNegs = \form -> case form of
                      T -> T
                      F -> F
                      Neg phi -> removeNegs phi
                      Cnj phi psi -> Cnj (removeNegs phi) (removeNegs psi)
                      Dsj phi psi -> Dsj (removeNegs phi) (removeNegs psi)

-- The type 'Bool' is already defined for us like this:
-- data Bool = True | False deriving Show

denotation :: Form -> Bool
denotation = \form -> case form of
                      T -> True
                      F -> False
                      Neg phi -> not (denotation phi)  -- case (denotation phi) of {True -> False; False -> True}
                      Cnj phi psi -> case (denotation phi) of {True -> denotation psi; False -> False}
                      Dsj phi psi -> case (denotation phi) of {True -> True; False -> denotation psi}

--------------------------------------

data Numb = Z | S Numb deriving Show

isZero :: Numb -> Bool
isZero = \n -> case n of {Z -> True; S n' -> False}

lessThanTwo :: Numb -> Bool
lessThanTwo = \n -> case n of
                    Z -> True
                    S x -> case x of {Z -> True; S y -> False}

double :: Numb -> Numb
double = \n -> case n of
               Z -> Z
               S n' -> S (S (double n'))

isOdd :: Numb -> Bool
isOdd = \n -> case n of
              Z -> False
              S n' -> not (isOdd n')

not :: Bool -> Bool
not = \b -> case b of {True -> False; False -> True}

one = S Z
two = S one
three = S two
four = S three
five = S four
six = S five
seven = S six

-- e.g. (add three) four ==>* seven
add :: Numb -> (Numb -> Numb)
add = \n -> (\m -> case n of
                   Z -> m
                   S n' -> S ((add n') m)
            )

----------------------------------------------

data IntList = Empty | NonEmpty Int IntList deriving Show

myList = NonEmpty 5 (NonEmpty 7 (NonEmpty 2 Empty))

total :: IntList -> Int
total = \l -> case l of
              Empty -> 0
              NonEmpty x rest -> x + total rest

-- 5, 7, 2
-- True, True, False
-- Dsj T F, Neg (Neg T), F, Cnj T (Dsj T F)

-- data BoolList = Empty | NonEmpty Bool BoolList deriving Show

otherTotal :: [Int] -> Int
otherTotal = \l -> case l of
                   [] -> 0
                   x : rest -> x + otherTotal rest





