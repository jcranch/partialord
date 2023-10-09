{-# LANGUAGE
      DerivingVia,
      PatternSynonyms,
      StandaloneDeriving
  #-}

-- | Partial orders
module Data.PartialOrd (
  PartialOrdering(..),
  FullyOrd(..),
  fromOrd,
  toMaybeOrd,
  fromMaybeOrd,
  comparable,
  Maxima(..),
  maxima,
  Minima(..),
  minima,
  ) where

import Prelude hiding ((<=), (>=))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Monoid ()
import Data.Semigroup ()
import Data.Set (Set)
import qualified Data.Set as S


-- | A data type representing relationships between two objects in a
-- poset: they can be related ('Just'), or unrelated ('Nothing').
--
-- We provide several patterns to make it more natural to work with
data PartialOrdering = EQ' | LT' | GT' | NT'
  deriving (Eq, Show)

fromOrd :: Ordering -> PartialOrdering
fromOrd EQ = EQ'
fromOrd LT = LT'
fromOrd GT = GT'

toMaybeOrd :: PartialOrdering -> Maybe Ordering
toMaybeOrd EQ' = Just EQ
toMaybeOrd LT' = Just LT
toMaybeOrd GT' = Just GT
toMaybeOrd NT' = Nothing

fromMaybeOrd :: Maybe Ordering -> PartialOrdering
fromMaybeOrd (Just EQ) = EQ'
fromMaybeOrd (Just LT) = LT'
fromMaybeOrd (Just GT) = GT'
fromMaybeOrd Nothing   = NT'


-- | A helper type for constructing partial orderings (using deriving
-- via): if something is totally ordered, it has a partial ordering
-- where everything is 'Just'.
newtype FullyOrd a = FullyOrd {
  getOrd :: a
} deriving (Eq, Ord, Show)

instance (Ord a) => PartialOrd (FullyOrd a) where
  compare' (FullyOrd x) (FullyOrd y) = fromOrd $ compare x y

-- | This one comes up so frequently it's worth hardwiring
deriving via FullyOrd Int instance PartialOrd Int


-- | A monoidal structure suitable for working with tuples.
--
-- It will turn out that (x1,y1) is less than or equal to (x2,y2) if
-- x1 is less than or equal to x2 and y1 is less than or equal to
-- y2. This monoidal structure combines information in this way.
instance Semigroup PartialOrdering where
  NT' <> _   = NT'
  EQ' <> x   = x
  _   <> NT' = NT'
  x   <> EQ' = x
  LT' <> LT' = LT'
  GT' <> GT' = GT'
  _   <> _   = NT'

instance Monoid PartialOrdering where
  mempty = EQ'

-- | A typeclass expressing partially ordered types: any two elements
-- are related by a `PartialOrdering`.
class PartialOrd a where
  {-# MINIMAL compare' | (<=) #-}

  compare' :: a -> a -> PartialOrdering
  compare' a b = case a <= b of
    True -> case a >= b of
      True -> EQ'
      False -> LT'
    False -> case a >= b of
      True -> GT'
      False -> NT'

  (<=) :: a -> a -> Bool
  a <= b = case compare' a b of
    LT' -> True
    EQ' -> True
    _   -> False

  (>=) :: a -> a -> Bool
  a >= b = b <= a


comparable :: PartialOrd a => a -> a -> Bool
comparable a b = case compare' a b of
  NT' -> False
  _   -> True


instance PartialOrd () where
  compare' _ _ = EQ'

instance (PartialOrd a, PartialOrd b) => PartialOrd (a,b) where
  compare' (a1,b1) (a2,b2) = case compare' a1 a2 of
    NT' -> NT'
    EQ' -> compare' b1 b2
    LT' -> if b1 <= b2 then LT' else NT'
    GT' -> if b1 >= b2 then GT' else NT'
  (a1,b1) <= (a2,b2) = a1 <= a2 && b1 <= b2

instance (PartialOrd a, PartialOrd b, PartialOrd c) => PartialOrd (a,b,c) where
  compare' (a1,b1,c1) (a2,b2,c2) = compare' ((a1,b1),c1) ((a2,b2),c2)
  (a1,b1,c1) <= (a2,b2,c2) = a1 <= a2 && b1 <= b2 && c1 <= c2

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d) => PartialOrd (a,b,c,d) where
  compare' (a1,b1,c1,d1) (a2,b2,c2,d2) = compare' (((a1,b1),c1),d1) (((a2,b2),c2),d2)
  (a1,b1,c1,d1) <= (a2,b2,c2,d2) = a1 <= a2 && b1 <= b2 && c1 <= c2 && d1 <= d2

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e) => PartialOrd (a,b,c,d,e) where
  compare' (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = compare' ((((a1,b1),c1),d1),e1) ((((a2,b2),c2),d2),e2)
  (a1,b1,c1,d1,e1) <= (a2,b2,c2,d2,e2) = a1 <= a2 && b1 <= b2 && c1 <= c2 && d1 <= d2 && e1 <= e2


instance Ord a => PartialOrd (Set a) where
  (<=) = S.isSubsetOf

  compare' u v = case compare (S.size u) (S.size v) of
    LT -> if S.isSubsetOf u v then LT' else NT'
    GT -> if S.isSubsetOf v u then GT' else NT'
    EQ -> if u == v then EQ' else NT'

instance PartialOrd IntSet where
  (<=) = IS.isSubsetOf

  compare' u v = case compare (IS.size u) (IS.size v) of
    LT -> if IS.isSubsetOf u v then LT' else NT'
    GT -> if IS.isSubsetOf u v then GT' else NT'
    EQ -> if u == v then EQ' else NT'


-- | Sets of incomparable elements, with a monoidal structure obtained
-- by taking the maximal ones.
--
-- Unfortunately, we need a full ordering for these to work (since
-- they use sets), though we don't assume they extend the given
-- partial order, or anything like that.  The monoid structures are
-- most efficient with pre-reduced sets as the left-hand argument.
newtype Maxima a = Maxima { maxSet :: Set a }

instance (Ord a, PartialOrd a) => Semigroup (Maxima a) where
  Maxima s1 <> Maxima s2 = let
    noLarger s x = not . any ((== LT') . compare' x) $ S.toList s
    s2' = S.filter (noLarger s1) s2
    s1' = S.filter (noLarger s2') s1
    in Maxima $ S.union s1' s2'

instance (Ord a, PartialOrd a) => Monoid (Maxima a) where
  mempty = Maxima S.empty
  mappend = (<>)

-- | Find the maxima of a list (passing it through the machinery above)
maxima :: (Ord a, PartialOrd a) => [a] -> [a]
maxima = S.toList . maxSet . mconcat . fmap (Maxima . S.singleton)


-- | As above, but with minima
newtype Minima a = Minima { minSet :: Set a }

instance (Ord a, PartialOrd a) => Semigroup (Minima a) where
  Minima s1 <> Minima s2 = let
    noSmaller s x = not . any ((== GT') . compare' x) $ S.toList s
    s2' = S.filter (noSmaller s1) s2
    s1' = S.filter (noSmaller s2') s1
    in Minima $ S.union s1' s2'

instance (Ord a, PartialOrd a) => Monoid (Minima a) where
  mempty = Minima S.empty
  mappend = (<>)

-- | Find the minima of a list (passing it through the machinery above)
minima :: (Ord a, PartialOrd a) => [a] -> [a]
minima = S.toList . minSet . mconcat . fmap (Minima . S.singleton)

