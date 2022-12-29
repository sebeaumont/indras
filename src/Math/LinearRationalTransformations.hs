
{-# LANGUAGE StrictData #-}

module Math.LinearRationalTransformations where

import Data.Complex
  (Complex((:+)), conjugate, magnitude, imagPart)

-- | Not sure how I want to proceed with fields
type CC = Complex Double

-- meanwhile

-- | complex unit
i :: CC
i = 0 :+ 1

one :: CC
one = 1 :+ 0

zero :: CC
zero = 0 :+ 0

cinv :: CC -> CC
cinv z = z' / (z * z')
  where z' = conjugate z

-- | Linear rational tranformations require four coefficients to give a
-- function in `z` such that: z |-> a + bz / c + dz; here we
-- define a type to encapsulate the parameters to the transformation.
-- which are really a 2x2 matrix.

data Mobius = LRT CC CC CC CC
  deriving (Eq)

instance Show Mobius where
  show (LRT a b c d) =
    "\n| " <> show a <> ", " <> show b <> "\n| " <> show c <> ", " <> show d

-- | Constructor for the transformation
mobius :: CC -> CC -> CC -> CC -> Mobius
mobius a b c d = LRT a b c d

-- | Identity transformation
identity :: Mobius
identity = mobius 1 0 0 1

-- | Apply the Mobius transformation to `z`
($$) :: Mobius -> CC -> CC
($$) (LRT a b c d) z = (a * z + b) / (c * z + d)
infixr 2 $$

-- | The Mobius tranformation forms a group
instance Semigroup Mobius where
  -- * Under matrix multiplication
  (LRT a b c d) <> (LRT a' b' c' d') =
    LRT
    (a * a' + b * c') (a * b' + b * d')
    (c * a' + d * c') (c * b' + d * d')

instance Monoid Mobius where
  mempty = identity -- ^ With the identity matrix

-- | Determinant of the transformation
determinant :: Mobius -> CC
determinant (LRT a b c d) = a * b - c * d
{-# INLINABLE determinant  #-}

-- | Make the determinant 1
normalize :: Mobius -> Mobius
normalize m@(LRT a b c d) =
  LRT (a * w) (b * w) (c * w) (d * w)
  where
    w :: CC
    w =  cinv . sqrt . determinant $ m
{-# INLINABLE normalize  #-}

inverse :: Mobius -> Mobius
inverse m@(LRT a b c d) =
  LRT (d * w) (-b * w) (-c * w) (a * w)
  where
    w :: CC
    w = cinv . determinant $ m
{-# INLINABLE inverse  #-}

trace :: Mobius -> CC
trace (LRT a _ _ d) = a + d
{-# INLINABLE trace  #-}

multiplier :: Mobius -> CC
multiplier m@(LRT a _ c _) =
  (a - c*r1) / (a - c*r2)
  where
    Fixpoints (r1, r2) = fixpoints m
{-# INLINABLE multiplier  #-}

-- | Pairs of complex roots
type Roots = (CC,CC) 

-- | TODO: Fixpoints can be at inf
newtype Fixpoints = Fixpoints Roots
  deriving (Eq, Show)

-- | Compute the fixpoints
fixpoints :: Mobius -> Fixpoints
fixpoints (LRT a b c d) =
  Fixpoints $ quadraticRoots c (d-a) (-b)
  

-- | Find roots of quadratic: /ax^2 + bx + c/
quadraticRoots :: CC -> CC -> CC -> Roots
quadraticRoots a b c = (r1,r2)
  where
    p2 = b / (2*a) 
    q = c / a
    r1 = -p2 + sqrt (p2*p2 - q)
    r2 = -(2*p2 + r1)

-- | TODO: Classification of fix points
data TransformClass
  = Elliptic Fixpoints
  | Parabolic Fixpoints
  | Loxodromic Fixpoints
  | Hyperbolic Fixpoints
  deriving (Eq, Show)

-- | Very confusing algorithm in the book 
classify :: Mobius -> TransformClass
classify m@(LRT a _ c _)
  | tr < (-2) || tr > 2 && k' > 1 =
      if isReal k then Hyperbolic r
      else Loxodromic r
  | otherwise =
    if k' == 1 then Elliptic r
    else Parabolic r
  where
    tr = magnitude $ trace m
    k' = magnitude k
    k = (a - c*r1) / (a - c*r2)
    r@(Fixpoints (r1, r2)) = fixpoints m
    isReal = (==0) . imagPart

-- | Grandma's recipe 
grandma :: CC -> CC -> (Mobius, Mobius)
grandma ta tb = (a,b)
  where
    tab = snd $ quadraticRoots 1 (-ta*tb) (ta*ta + tb*tb)
    z0  = (tab - 2)*tb / (tb*tab - 2*ta + 2*i*tab)
    b   = LRT ((tb-2*i)/2) (tb/2) (tb/2) ((tb+2*i)/2)
    ab  = LRT (tab/2) ((tab-2)/(2*z0)) ((tab+2)*z0/2) (tab/2)
    a   = ab `mappend` inverse b

-- | conjugate of t with the conjugating map s 
conj :: Mobius -> Mobius -> Mobius
conj s t = s <> t <> inverse s


