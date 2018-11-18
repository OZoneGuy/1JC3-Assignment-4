{- Assignment 4 Extra Credit
 - Name: Omar Alkersh
 - Date: TODO add of completion
 -}
module Assign_4_ExtraCredit where

import Criterion.Main

macid = "alkersho"


data Nat = Z
         | S Nat
         deriving Show

data Digit = Zero
           | One deriving Show

data BinNat = Atom Digit
            | Compound BinNat Digit
            deriving Show

plus :: Nat -> Nat -> Nat
plus Z y = y
plus (S a) y = S (plus a y)

time :: Nat -> Nat -> Nat
time a (S b) = plus (time a b) a
time _ _ = Z

--doesn't work for more complex binary numbers
--runs continously and never stops
binPlus :: BinNat -> BinNat -> BinNat
binPlus (Atom Zero) a = a
binPlus a (Atom Zero) = a
binPlus (Atom One) (Atom One) = Compound (Atom One) Zero
binPlus (Atom One) (Compound a b) = binPlus (Compound a Zero) (binPlus (Atom b) (Atom One))
binPlus (Compound a b) (Atom One) = binPlus (Compound a Zero) (binPlus (Atom b) (Atom One))
binPlus (Compound a b) (Compound x y) = binPlus (Compound (binPlus a x) Zero) (binPlus (Atom b) (Atom y))

--affected by the same bug as binPlus
binTime :: BinNat -> BinNat -> BinNat
binTime (Atom Zero) _ = Atom Zero
binTime _ (Atom Zero) = Atom Zero
binTime a (Atom One) = a
binTime (Atom One) a = a
binTime (Compound a b) (Compound x y) = binPlus (Compound (Compound (binTime a x) Zero) Zero) $ binPlus (Compound a Zero) $ binPlus (Compound x Zero) $ binTime (Atom b) (Atom y)
