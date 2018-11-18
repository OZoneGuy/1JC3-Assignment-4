{- Assignment 4
 - Name: Omar Alkersh
 - Date: 18/11/18
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
simplify (PolyList xs) | last xs == 0 = simplify (PolyList (init xs))
                       | otherwise = PolyList xs

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
  in PolyList $ func xs ys

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
 - - Function: getPolyList
 - - Test Case Number: 1-A
 - - Input: File ->
 {50
 10
 0}
 - - Expected Output: PolyList [50,10]
 - - Acutal Output: PolyList [50,10]
 - -----------------------------------------------------------------
 - - - Function: getPolyList
 - - Test Case Number: 1-B
 - - Input: File ->
 {0
  10
  -50}
 - - Expected Output: PolyList [0,10, -50]
 - - Acutal Output: PolyList [0,10,-50]
 - -----------------------------------------------------------------
 - - Function: getPolyList
 - - Test Case Number: 1-C
 - - Input: File ->
 {25
 -10
 50}
 - - Expected Output: PolyList [25,-10, 50]
 - - Acutal Output: PolyList [25,-10, 50]
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Function: getPolyList
 - Property: None defined
 - Actual Test Result: NA
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 2-A
 - - Input: PolyList [1,2,3] 5
 - - Expected Output: 86
 - - Acutal Output: 86
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 2-B
 - - Input: PolyList [-1,-2,0] 5
 - - Expected Output: -11
 - - Acutal Output: -11
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 2-C
 - - Input: PolyList [-1,-2,-3] 5
 - - Expected Output: -86
 - - Acutal Output: -86
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Function: polyListValue
 - Property: None defined
 - Actual Test Result: NA
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 3-A
 - - Input: PolyList [1,2,3,0]
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 3-B
 - - Input: PolyList [1,2,0,3]
 - - Expected Output: 3
 - - Acutal Output: 3
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 3-C
 - - Input: PolyList [1,5,3,7,-5]
 - - Expected Output: 4
 - - Acutal Output: 4
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Function: polyListDegree
 - Property: null xs || toInteger( length xs - 1) == polyListDegree (PolyList xs)
 - Actual Test Result: PASS
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - - Function: polyListDeriv
 - - Test Case Number: 4-A
 - - Input: PolyList [1,2,3]
 - - Expected Output: PolyList [2,6]
 - - Acutal Output: PolyList [2,6]
 - -----------------------------------------------------------------
 - - Function: polyListDeriv
 - - Test Case Number: 4-B
 - - Input: PolyList [1,2,3,0]
 - - Expected Output: PolyList [2,6]
 - - Acutal Output: PolyList [2,6]
 - -----------------------------------------------------------------
 - - Function: polyListDeriv
 - - Test Case Number: 4-C
 - - Input: polyListDeriv [1,2,3,0]
 - - Expected Output: PolyList [2,6]
 - - Acutal Output: PolyList [2,6]
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Function: polyListDeriv
 - Property: null xs || (length xs - 1 == length (getList (polyListDeriv (PolyList xs))))
 - Actual Test Result: PASS
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 5-A
 - - Input: (PolyList [1,2,3]) (PolyList [1,2,3])
 - - Expected Output: PolyList [2,4,6]
 - - Acutal Output: PolyList [2,4,6]
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 5-B
 - - Input: (PolyList [1,2,3]) (PolyList [1,2,3,4])
 - - Expected Output: PolyList [2,4,6,4]
 - - Acutal Output: PolyList [2,4,6,4]
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 5-C
 - - Input: (PolyList [1,2,3]) (PolyList [1,2,-3])
 - - Expected Output: PolyList [2,4]
 - - Acutal Output: PolyList [2,4]
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Function: polyListSumProp1
 - Property: let
  polySum = polyListSum (PolyList xs) (PolyList ys)
  polySumV = polyListValue polySum n
  poly1V = polyListValue (PolyList xs) n
  poly2V = polyListValue (PolyList ys) n
  in (null xs || null ys) || abs(polySumV - (poly1V + poly2V)) < 10E-1
 - Actual Test Result: FAIL
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 6-A
 - - Input: (PolyList [1,2,3]) (PolyList [1,2,3])
 - - Expected Output: PolyList [1,4,10,12,9]
 - - Acutal Output: PolyList [1,4,10,12,9]
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 6-B
 - - Input: (PolyList [1,0,3]) (PolyList [1,2,3])
 - - Expected Output: PolyList [1,4,6,6,9]
 - - Acutal Output: PolyList [1,4,6,6,9]
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 6-C
 - - Input: (PolyList [-1,2,-3]) (PolyList [1,2,3])
 - - Expected Output: PolyList [-1,0,-2,0,-9]
 - - Acutal Output: PolyList [-1,0,-2,0,-9]
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Function: polyListDegree
 - Property: let
  polyProd = polyListProd (PolyList xs) (PolyList ys)
  polyProdV = polyListValue polyProd n
  poly1V = polyListValue (PolyList xs) n
  poly2V = polyListValue (PolyList ys) n
  in (null xs || null ys) || abs(polyProdV - (poly1V * poly2V)) < 10E-2
 - Actual Test Result: FAIL
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 7-A
 - - Input: polyList [1,2,3]
 - - Expected Output: Sum (Prod X (Prod X (Coef 3))) (Sum (Prod X (Coef 2)) (Coef 1))
 - - Acutal Output: Sum (Prod X (Prod X (Coef 3))) (Sum (Prod X (Coef 2)) (Coef 1))
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 7-B
 - - Input: PolyList [1,0,3]
 - - Expected Output: Sum (Prod X (Prod X (Coef 3))) (Sum (Prod X (Coef 0)) (Coef 1))
 - - Acutal Output: Sum (Prod X (Prod X (Coef 3))) (Sum (Prod X (Coef 0)) (Coef 1))
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 7-C
 - - Input: PolyList [-1,2,-3]
 - - Expected Output: Sum (Prod X (Prod X (Coef -3))) (Sum (Prod X (Coef 2)) (Coef -1))
 - - Acutal Output: Sum (Prod X (Prod X (Coef -3))) (Sum (Prod X (Coef 2)) (Coef -1))
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Function: polyListDegree
 - Property: let
   polyList = PolyList xs
   in null xs || (polyList == polyToPolyList(polyListToPoly polyList))
 - Actual Test Result: PASS
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 8-A
 - - Input: Sum (Prod X (Prod X (Coef 3))) (Sum (Prod X (Coef 2)) (Coef 1))
 - - Expected Output: PolyList [1,2,3]
 - - Acutal Output: PolyList [1,2,3]
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 8-B
 - - Input: Sum (Prod X (Prod X (Coef 3))) (Sum (Prod X (Coef 0)) (Coef 1))
 - - Expected Output: PolyList [1,0,3]
 - - Acutal Output: PolyList [1,0,3]
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 8-C
 - - Input: Sum (Prod X (Prod X (Coef 3))) (Sum (Prod X (Coef (-2))) (Coef 1))
 - - Expected Output: PolyList [1,-2,3]
 - - Acutal Output: PolyList [1,-2,3]
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Function: polyListDegree
 - Property: let
   polyList = PolyList xs
   in null xs || (polyList == polyToPolyList(polyListToPoly polyList))
 - Actual Test Result: PASS
 - -----------------------------------------------------------------
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
 in (null xs || null ys) || abs(polySumV - (poly1V + poly2V)) < 10E-1

polyListProdProp1 :: [Float] -> [Float] -> Float -> Bool
polyListProdProp1 xs ys n = let
 polyProd = polyListProd (PolyList xs) (PolyList ys)
 polyProdV = polyListValue polyProd n
 poly1V = polyListValue (PolyList xs) n
 poly2V = polyListValue (PolyList ys) n
 in (null xs || null ys) || abs(polyProdV - (poly1V * poly2V)) < 10E-2

polyListToPolyProp1 :: [Float] -> Bool
polyListToPolyProp1 xs = let
  polyList = PolyList xs
  in null xs || (polyList == polyToPolyList(polyListToPoly polyList))
