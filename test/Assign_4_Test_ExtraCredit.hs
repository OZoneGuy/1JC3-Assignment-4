{- Assignment 4 Tests
 - Name: Omar Alkersh
 - Date: 28/11/18
 -}

import Assign_4
import Assign_4_ExtraCredit

import Test.QuickCheck
import Criterion.Main   -- see https://www.stackage.org/haddock/lts-8.24/criterion-1.1.4.0/Criterion-Main.html

tenNat :: Nat
tenNat = S(S(S(S(S(S(S(S(S(S Z)))))))))
tenBinNat = Compound (Compound (Compound (Atom One) Zero) One) Zero

main :: IO ()
main = defaultMain [
  bgroup "NatPlus" [
    bench "10+1" $ whnf (plus tenNat) (S Z),
    bench "10+5" $ whnf (plus tenNat) (S(S(S(S(S Z))))),
    bench "10+10" $ whnf (plus tenNat) tenNat,
    bench "10+20" $ whnf (plus tenNat) (S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S Z))))))))))))))))))))
    ],
    bgroup "NatTime"[
    bench "10*1" $ whnf (time tenNat) (S Z),
    bench "10*5" $ whnf (time tenNat) (S(S(S(S(S Z))))),
    bench "10*10" $ whnf (time tenNat) tenNat,
    bench "10*20" $ whnf (time tenNat) (S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S Z))))))))))))))))))))
    ],
    bgroup "BinNatPlus"[
      bench "10+1" $ whnf (binPlus tenBinNat) (Atom One),
      bench "10+5" $ whnf (binPlus tenBinNat) (Compound(Compound(Atom One)Zero)Zero),
      bench "10+10" $ whnf (binPlus tenBinNat) tenBinNat,
      bench "10+20" $ whnf (binPlus tenBinNat) (Compound(Compound(Compound(Compound (Atom One)Zero)One)Zero)Zero)
    ],
    bgroup "BinNatTime"[
    bench "10*1" $ whnf (binTime tenBinNat) (Atom One),
    bench "10*5" $ whnf (binTime tenBinNat) (Compound(Compound(Atom One)Zero)Zero),
    bench "10*10" $ whnf (binTime tenBinNat) tenBinNat,
    bench "10*20" $ whnf (binTime tenBinNat) (Compound(Compound(Compound(Compound (Atom One)Zero)One)Zero)Zero)
    ]
  ]
