{- Assignment 4
 - Name: Omar Alkersh
 - Date: TODO add of completion
 -}
module Assign_4 where

macid :: String
macid = "alkersho"


data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
            deriving Show

newtype PolyList a = PolyList [a] deriving Show

{- -----------------------------------------------------------------
 - getPolyList
 - -----------------------------------------------------------------
 - Description: TODO add comments on getPolyList here
 -}
getPolyList :: FilePath -> IO (PolyList Integer)
getPolyList file = do
  fileText <- readFile file
  let
    text = lines fileText
    values = map (\x -> read x::Integer)
  return (PolyList (values text))


{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListValue here
 -}
polyListValue :: Num a => PolyList a -> a -> a
polyListValue (PolyList [])  _ = 0
polyListValue (PolyList [x]) _ = x
polyListValue (PolyList (x:xs)) n = x + n * polyListValue (PolyList xs) n

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListDegree here
 -}
polyListDegree :: Num a => PolyList a -> Integer
polyListDegree (PolyList []) = undefined
polyListDegree (PolyList xs) = toInteger(length xs) - 1

{- -----------------------------------------------------------------
 - polyListDeriv
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListDeriv here
 -}
polyListDeriv :: (Num a, Enum a)=> PolyList a -> PolyList a
polyListDeriv (PolyList (x:xs)) = let
  derive:: Num a => [a] -> a -> [a]
  derive (x:xs) n = x*n:derive xs (n+1)
  derive _ _ = []
  in PolyList (derive xs 1)
-- polyListDeriv (PolyList xs) = PolyList (tail (zipWith (*) xs [0..]))

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListSum here
 -}
polyListSum :: Num a => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList xs) (PolyList ys) = PolyList (sumList xs ys)

sumList:: Num a => [a] -> [a] -> [a]
sumList (x:xs) (y:ys) = (y+x):sumList xs ys
sumList [] ys | not(null ys) = ys
sumList xs [] | not(null xs) = xs
sumList _ _ = []



{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListProd here
 -}
polyListProd :: Num a => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList xs) (PolyList ys) = let
    func [x] ys1 = map(*x) ys1
    func (x:xs1) ys1 = let
      comp = map (*x) ys1
      in head comp : sumList (tail comp) (func xs1 ys1)
  in PolyList (func xs ys)

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyListToPoly here
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly p1 = error "TODO: implement polyListToPoly"

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: TODO add comments on polyToPolyList here
 -}
polyToPolyList :: Num a => Poly a -> PolyList a
polyToPolyList X = PolyList [0,1]
polyToPolyList (Coef x) = PolyList [x]
polyToPolyList (Sum a b) = polyListSum (polyToPolyList  a) (polyToPolyList b)
polyToPolyList (Prod a b) = polyListProd (polyToPolyList a) (polyToPolyList b)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function:
 - - Test Case Number:
 - - Input:
 - - Expected Output:
 - - Acutal Output:
 - -----------------------------------------------------------------
 - TODO: add test cases
 -}
