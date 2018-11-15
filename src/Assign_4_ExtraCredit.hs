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

binPlus :: BinNat -> BinNat -> BinNat
binPlus (Atom Zero) a = a
binPlus a (Atom Zero) = a
binPlus (Atom One) (Atom One) = Compound (Atom One) Zero
binPlus (Compound a One) (Atom One) = Compound (binPlus a (Atom One)) Zero
binPlus (Compound a Zero) (Atom One) = Compound a One
binPlus (Compound a One) (Compound x One) = Compound (Compound (binPlus a x) One) One
binPlus (Compound a b) (Compound x Zero) = Compound (binPlus a x) b
binPlus (Compound a Zero) (Compound x y) = Compound (binPlus a x) y

binTime :: BinNat -> BinNat -> BinNat
binTime a b = error "needs to be implemented"
