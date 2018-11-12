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
          quickCheck prop1
          -- TODO implement real tests

prop1 :: Int -> Bool
prop1 _ = True
