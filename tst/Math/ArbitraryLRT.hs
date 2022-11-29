module Math.ArbitraryLRT
  ( module Math.LinearRationalTransformations
  ) where 

import Test.QuickCheck (Arbitrary, arbitrary)
import Math.LinearRationalTransformations

-- TODO newtype wrapper to avoid orphans

instance Arbitrary Mobius where
  arbitrary =
    mobius <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 
    
