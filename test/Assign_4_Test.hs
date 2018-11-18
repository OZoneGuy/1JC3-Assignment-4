{- Assignment 4 Tests
 - Name: TODO add full name
 - Date: TODO add of completion
 -}

import Assign_4
import Assign_4_ExtraCredit

import Test.QuickCheck
import Criterion.Main   -- see https://www.stackage.org/haddock/lts-8.24/criterion-1.1.4.0/Criterion-Main.html

main :: IO ()
main = do print "Performing Test 1: "
          quickCheck polyListDerivProp1
          print "Performing Test 2: "
          quickCheck polyListSumProp1
          print "Performing Test 3: "
          quickCheck polyListProdProp1
          print "Performing Test 4: "
          quickCheck polyListDegreeProp1
          print "Performing Test 5: Cannot test getPolyList"
          -- quickCheck getPolyListProp1
          print "Performing Test 6: "
          quickCheck polyListToPolyProp1
