{-# LANGUAGE DeriveAnyClass #-}
{-|
Module:   Math.LRTDenotationQQ
Synopsis: Denotation of an LRT over Q x Q
Author:   Simon Beaumont
License:  BSD3
Description:

Define the essential semantics and algebra of linear rational
transforms of the (Q rational) complex plane.
-}
module Math.LRTDenotationQQ where

import Data.Complex
import Data.Ratio

-- | Complex rational numbers
newtype Q = Q (Complex Rational)

-- | Rather basic definition of multiplication of complex rationals
-- necessary to have compiler derive abstractions for pairs in QQ.  
instance Semigroup Q where
  (Q (re :+ im)) <> (Q (re' :+ im')) = 
    let
      ra = numerator re
      rb = denominator re
      ia = numerator im
      ib = denominator im
      ra' = numerator re'
      rb' = denominator re'
      ia' = numerator im'
      ib' = denominator im'

      f = (ra * ra') % (rb * rb')
      o = (ra * ia') % (rb * ib')
      i = (ia * ra') % (ib * rb')
      l = (ia * ia') % (ib * ib')

    in Q ((f - l) :+ (o + i))

-- | The multiplicative identity for Q
instance Monoid Q where
  mempty = Q (1 :+ 1)

-- | TODO: Need a (Num Q) instance for arithmetic

--------------------------------------------------------------------------------
-- | A map is a function!
type Map f = f -> f

-- | The type of a general transformation.
data T = T (Map Q)

-- | Associative by function composition
instance Semigroup T where
  (T f) <> (T g) = T (f . g)
-- | With a multiplicative identity
instance Monoid T where
  mempty = T id

-- | LRTs or Möbius maps or Linear Rational Transformations (LRT) are more
-- are more restricted than the most general denotation given here, and have
-- four parameters: z |-> a z + b / c z + d
-- forall complex numbers z, a, b, c, d and not just some arbitrary function.

-- | So LRTs are a family of functions:
{-
mobius :: Q -> Q -> Q -> Q -> T
mobius a b c d = T (\z -> (a * z + b) / (c * z + c))
-}

  
