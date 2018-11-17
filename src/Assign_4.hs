{- Assignment 4
 - Name: Omar Alkersh
 - Date: TODO add of completion
 -}
module Assign_4 where

import Test.QuickCheck.Monadic

macid :: String
macid = "alkersho"


data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
            deriving Show

newtype PolyList a = PolyList [a] deriving (Show, Eq)

simplify::(Num a, Eq a) =>  PolyList a -> PolyList a
simplify (PolyList (x:xs)) | x == 0    = simplify (PolyList xs)
                           | otherwise = PolyList (x:xs)

{- -----------------------------------------------------------------
 - getPolyList
 - -----------------------------------------------------------------
 - reads a file containing numbers::Integers, 1 each line, and turns them into
 - a PolyList value, each line corresponds to each monomial
 -}
getPolyList :: FilePath -> IO (PolyList Integer)
getPolyList file = do
  fileText <- readFile file
  let
    values = map (\x -> read x::Integer) (lines fileText)
  return $ simplify $ PolyList values


{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - substitudes a value into the polynomial.
 - Uses Horner's method - https://en.wikipedia.org/wiki/Horner%27s_method
 - first iteration, adds the first monomial, a*x^0, to the value multiplied
 - by the second value,
 - recursevly repeats until reaches base case, one element in PolyList
 - a0+x*(a1+x*(...(an+x))) = a0+a1*x+a2*x^2+...+an*x^n
 -}
polyListValue :: (Num a, Eq a) => PolyList a -> a -> a
polyListValue (PolyList [x]) _ = x
polyListValue (PolyList (x:xs)) n = x + n * polyListValue (PolyList xs) n

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: Finds the highest power in the polynomial
 - First checks if last element is 0
 - If true remove last element and recurse
 - otherwise returns length - 1 if length > 0
 - else returns undefined
 -}
polyListDegree :: (Num a, Eq a)=> PolyList a -> Integer
polyListDegree (PolyList []) = undefined
polyListDegree (PolyList xs) | last xs == 0 = polyListDegree (PolyList (init xs))
                             | otherwise = if null xs
                               then undefined
                               else toInteger(length xs) - 1

{- -----------------------------------------------------------------
 - polyListDeriv
 - -----------------------------------------------------------------
 - Description: Finds the derivative of the polynomial
 - removes first element and m ultiplies the remaining element by 1...n
 - respectively
 - Uses power rule
 -}
polyListDeriv :: (Num a, Eq a)=> PolyList a -> PolyList a
polyListDeriv (PolyList (x:xs)) = let
  derive:: Num a => [a] -> a -> [a]
  derive (x:xs) n = x*n:derive xs (n+1)
  derive _ _ = []
  in simplify $ PolyList $ derive xs 1

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: Adds the corresponding monmials, first with first etc.
 - uses auxilarry function sumList
 -}
polyListSum :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList xs) (PolyList ys) = simplify $ PolyList $ sumList xs ys

-- made because zipWith get rid of excess elements, this keeps excess elements
sumList:: Num a => [a] -> [a] -> [a]
sumList (x:xs) (y:ys) = (y+x):sumList xs ys
sumList [] ys = ys
sumList xs [] = xs

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: multiples two polynomials together
 - Take the first element of list xs and multiples it with list ys giving xs1
 - takes the tail of result and adds it to the product of the secod element and ys giving xs2
 - combines the head of xs1 with xs2
 - does ths recursevely untill all elements of xs are used
 -}
polyListProd :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList xs) (PolyList ys) = let
    func [x] ys1 = map(*x) ys1
    func (x:xs1) ys1 = let
      comp = map (*x) ys1
      in head comp : sumList (tail comp) (func xs1 ys1)
  in simplify $ PolyList $ func xs ys

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: Turns PolyList type into Poly
 - uses auxilarry function, aux, to turn individual monomials into into Poly type
 -
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList [x]) = Coef x
polyListToPoly (PolyList xs)  = let
  l = length xs - 1
  aux :: Num a => a -> Int -> Poly a
  aux x 0 = Coef x
  aux x n = Prod X (aux x (n-1))
  in Sum (aux (last xs) l) (polyListToPoly (PolyList (init xs)))

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: Turns Poly type into PolyList
 - uses polyListSum and polyListProd to turn (sum a b) (Prod a b) into PolyList respectively
 -}
polyToPolyList :: (Num a, Eq a) => Poly a -> PolyList a
polyToPolyList (Coef x) = PolyList [x]
polyToPolyList X = PolyList [0,1]
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

getList :: PolyList a -> [a]
getList (PolyList xs) = xs


getPolyListProp1 f = monadicIO $ do
  text <- run $ readFile f
  poly <- run $ getPolyList f
  let
    lineC = length (lines text)
  assert $ lineC == length (getList poly)


polyListDegreeProp1 :: [Double] -> Bool
polyListDegreeProp1 xs = null xs || toInteger( length xs - 1) == polyListDegree (PolyList xs)

polyListDerivProp1 :: [Double] -> Bool
polyListDerivProp1 xs = null xs || (length xs - 1 == length (getList (polyListDeriv (PolyList xs))))

polyListSumProp1 :: [Double] -> [Double] -> Double -> Bool
polyListSumProp1 xs ys n = let
 polySum = polyListSum (PolyList xs) (PolyList ys)
 polySumV = polyListValue polySum n
 poly1V = polyListValue (PolyList xs) n
 poly2V = polyListValue (PolyList ys) n
 in (null xs || null ys) || (abs(polySumV - (poly1V + poly2V)) < 10E-10)

polyListProdProp1 :: [Float] -> [Float] -> Float -> Bool
polyListProdProp1 xs ys n = let
 polyProd = polyListProd (PolyList xs) (PolyList ys)
 polyProdV = polyListValue polyProd n
 poly1V = polyListValue (PolyList xs) n
 poly2V = polyListValue (PolyList ys) n
 in (null xs || null ys) || (polyProdV == (poly1V * poly2V))

polyListTopPolyProp1 :: [Float] -> Bool
polyListTopPolyProp1 xs = let
  polyList = PolyList xs
  in null xs || (polyList == polyToPolyList(polyListToPoly polyList))
