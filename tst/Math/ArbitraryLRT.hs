{-# LANGUAGE TemplateHaskell #-}
module Math.ArbitraryLRT
  (runTests) where

import Test.QuickCheck
import Math.ArbitraryReal

import qualified Math.LinearRationalTransformations
  as LRT

newtype AnyMobius = AnyMobius LRT.Mobius
  deriving (Show, Eq)

instance Arbitrary AnyMobius where
  arbitrary = do
    a <- arbitrary :: Gen ApproxComplex
    b <- arbitrary :: Gen ApproxComplex
    c <- arbitrary :: Gen ApproxComplex
    d <- arbitrary :: Gen ApproxComplex
    pure . AnyMobius $ LRT.mobius (unwrap a) (unwrap b) (unwrap c) (unwrap d)


prop_rightIdentity (AnyMobius t) = t <> LRT.identity === t 

prop_leftIdentity (AnyMobius t) = LRT.identity <> t === t 

prop_assoc (AnyMobius s) (AnyMobius t) (AnyMobius u)
  = s <> (t <> u) === (s <> t) <> u

newtype NonZeroMobius = NonZeroMobius LRT.Mobius
  deriving (Show, Eq)

instance Arbitrary NonZeroMobius where
  arbitrary = do
    a <- arbitrary :: Gen (NonZero LRT.CC)
    b <- arbitrary :: Gen (NonZero LRT.CC)
    c <- arbitrary 
    d <- arbitrary
    pure . NonZeroMobius $ LRT.mobius (getNonZero a) (getNonZero b) c d

prop_detNonZero (NonZeroMobius t) = LRT.determinant t =/= LRT.zero
            
prop_normalize (NonZeroMobius t) = (LRT.determinant . LRT.normalize) t === LRT.one

-- need to define Eq for AnyMobius in terms of normalised Equal(ity) 


return []
runTests = $quickCheckAll
