{-# LANGUAGE TemplateHaskell #-}
-- | Should test up to numerical stability that floating point behaves
-- something like real numbers.
module Math.ArbitraryReal where

import Numeric.IEEE (epsilon)
import Data.Complex
import Test.QuickCheck

-- | Digital logic is primitive stuff as of 2022
-- @see https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition

almostEqualRelativeAndAbs :: Double -> Double -> Double -> Double -> Bool
almostEqualRelativeAndAbs maxRelDiff maxDiff a b 
  | diff < maxDiff = True
  | diff <= largest * maxRelDiff = True
  | otherwise = False 
  where diff = abs (a - b)
        largest = max (abs a) (abs b)
  
almostEqualRelativeAndAbsEps :: Double -> Double -> Double -> Bool
almostEqualRelativeAndAbsEps = almostEqualRelativeAndAbs epsilon 

class Num a => Equal a where
  (~=) :: a -> a -> Bool
infix 4 ~=

instance Equal Double where
  -- XXX CAUTION YMMV XXX
  (~=) = almostEqualRelativeAndAbsEps 0.000000001

instance Equal (Complex Double) where
  z ~= w = realPart z ~= realPart w && imagPart z ~= imagPart w

 -- we need to test monoid (associativity and identity) at least
newtype ApproxComplex = AC {unwrap :: Complex Double}
  deriving (Show)


instance Arbitrary ApproxComplex where
  arbitrary = AC <$> arbitrary

instance Eq ApproxComplex where
  (AC z) == (AC w) = z ~= w

instance Num ApproxComplex where
  (AC z) + (AC w) = AC $ z + w
  (AC z) * (AC w) = AC $ z * w
  abs (AC z) = AC $ abs z
  signum (AC z) = AC $ signum z
  fromInteger = AC . fromInteger
  negate (AC z) = AC $ negate z

prop_refl :: ApproxComplex -> Property  
prop_refl (AC z) = z === z

prop_assocPlus :: ApproxComplex -> ApproxComplex -> ApproxComplex -> Property
prop_assocPlus z w u =
  ((z + w) + u) === (z + (w + u))

prop_assocMult :: ApproxComplex -> ApproxComplex -> ApproxComplex -> Property
prop_assocMult z w u =
  ((z * w) * u) === (z * (w * u))


return []
runRealTests :: IO Bool
runRealTests = $quickCheckAll
