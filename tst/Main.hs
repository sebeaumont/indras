module Main (main) where

import Test.QuickCheck 
import Math.ArbitraryLRT as LRT 

prop_rightIdentity :: LRT.Mobius -> Bool
prop_rightIdentity t = t <> LRT.ii == t 

prop_leftIdentity :: LRT.Mobius -> Bool
prop_leftIdentity t = LRT.ii <> t == t 

-- TODO automate the runner

main :: IO ()
main = do
  verboseCheck (prop_rightIdentity)
  verboseCheck (prop_leftIdentity)

