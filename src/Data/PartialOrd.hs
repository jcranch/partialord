{-# LANGUAGE
      DerivingVia,
      PatternSynonyms,
      StandaloneDeriving
  #-}

-- | Partial orders
module Data.PartialOrd (
  -- * Comparisons in partial orders
  PartialOrdering(..),
  fromOrd,
  toMaybeOrd,
  fromMaybeOrd,
  fromLeqGeq,
  fromCompare,
  -- * Partial orderings
  PartialOrd(..),
  comparable,
  -- * Special partial orderings
  FullyOrd(..),
  Discrete(..),
  -- * Maxima and minima
  Maxima(..),
  maxima,
  Minima(..),
  minima,
  -- * Partial orders on lists
  Infix(..),
  Prefix(..),
  Suffix(..),
  Subseq(..),
  ) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, isSubsequenceOf)
import Data.Monoid ()
import Data.Semigroup ()
import Data.Set (Set)
import qualified Data.Set as S


-- | A data type representing relationships between two objects in a
-- poset: they can be related (by EQ', LT' or GT'; like EQ, LT or GT),
-- or unrelated (NT').
data PartialOrdering = EQ' | LT' | GT' | NT'
  deriving (Eq, Show)

-- | Convert an ordering into a partial ordering
fromOrd :: Ordering -> PartialOrdering
fromOrd EQ = EQ'
fromOrd LT = LT'
fromOrd GT = GT'

-- | Lift a `compare` to a `compare'`
fromCompare :: Ord a => a -> a -> PartialOrdering
fromCompare x y = fromOrd $ compare x y

-- | Convert a partial ordering to an ordering
toMaybeOrd :: PartialOrdering -> Maybe Ordering
toMaybeOrd EQ' = Just EQ
toMaybeOrd LT' = Just LT
toMaybeOrd GT' = Just GT
toMaybeOrd NT' = Nothing

-- | Convert an ordering into a partial ordering
fromMaybeOrd :: Maybe Ordering -> PartialOrdering
fromMaybeOrd (Just EQ) = EQ'
fromMaybeOrd (Just LT) = LT'
fromMaybeOrd (Just GT) = GT'
fromMaybeOrd Nothing   = NT'

-- | Convert from `leq` and `geq` to a partial ordering
fromLeqGeq :: Bool -> Bool -> PartialOrdering
fromLeqGeq True True = EQ'
fromLeqGeq True False = LT'
fromLeqGeq False True = GT'
fromLeqGeq False False = NT'


-- | A helper type for constructing partial orderings from total
-- orderings (using deriving via)
newtype FullyOrd a = FullyOrd {
  getOrd :: a
} deriving (Eq, Ord, Show)

instance (Ord a) => PartialOrd (FullyOrd a) where
  compare' (FullyOrd x) (FullyOrd y) = fromOrd $ compare x y


-- | A helper type for constructing partial orderings where everything
-- is equal or incomparable.
newtype Discrete a = Discrete {
  getDiscrete :: a
} deriving (Eq, Show)

instance (Eq a) => PartialOrd (Discrete a) where
  compare' (Discrete x) (Discrete y)
    | x == y    = EQ'
    | otherwise = NT'


-- | A comparison (less than or equal, greater than or equal) holds if
-- and only if it does on both arguments.
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
  {-# MINIMAL compare' | leq #-}

  compare' :: a -> a -> PartialOrdering
  compare' a b = fromLeqGeq (a `leq` b) (a `geq` b)

  leq :: a -> a -> Bool
  a `leq` b = case compare' a b of
    LT' -> True
    EQ' -> True
    _   -> False

  geq :: a -> a -> Bool
  a `geq` b = b `leq` a

-- | Are they LT', EQ', GT'
comparable :: PartialOrd a => a -> a -> Bool
comparable a b = case compare' a b of
  NT' -> False
  _   -> True

-- | It's hard to imagine another sensible instance
deriving via FullyOrd Int instance PartialOrd Int

-- | It's hard to imagine another sensible instance
deriving via FullyOrd Integer instance PartialOrd Integer


instance PartialOrd () where
  compare' _ _ = EQ'

-- | This is equivalent to
--
--   >   compare' (a,b) (c,d) = compare' a c <> compare' b d
--
--   but may be more efficient: if compare' a c is LT' or GT' we need less
--   information about b and d.
instance (PartialOrd a, PartialOrd b) => PartialOrd (a,b) where
  compare' (a1,b1) (a2,b2) = case compare' a1 a2 of
    NT' -> NT'
    EQ' -> compare' b1 b2
    LT' -> if b1 `leq` b2 then LT' else NT'
    GT' -> if b1 `geq` b2 then GT' else NT'
  (a1,b1) `leq` (a2,b2) = a1 `leq` a2 && b1 `leq` b2

instance (PartialOrd a, PartialOrd b, PartialOrd c) => PartialOrd (a,b,c) where
  compare' (a1,b1,c1) (a2,b2,c2) = compare' ((a1,b1),c1) ((a2,b2),c2)
  (a1,b1,c1) `leq` (a2,b2,c2) = a1 `leq` a2 && b1 `leq` b2 && c1 `leq` c2

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d) => PartialOrd (a,b,c,d) where
  compare' (a1,b1,c1,d1) (a2,b2,c2,d2) = compare' (((a1,b1),c1),d1) (((a2,b2),c2),d2)
  (a1,b1,c1,d1) `leq` (a2,b2,c2,d2) = a1 `leq` a2 && b1 `leq` b2 && c1 `leq` c2 && d1 `leq` d2

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e) => PartialOrd (a,b,c,d,e) where
  compare' (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = compare' ((((a1,b1),c1),d1),e1) ((((a2,b2),c2),d2),e2)
  (a1,b1,c1,d1,e1) `leq` (a2,b2,c2,d2,e2) = a1 `leq` a2 && b1 `leq` b2 && c1 `leq` c2 && d1 `leq` d2 && e1 `leq` e2


instance Ord a => PartialOrd (Set a) where
  leq = S.isSubsetOf

  compare' u v = case compare (S.size u) (S.size v) of
    LT -> if S.isSubsetOf u v then LT' else NT'
    GT -> if S.isSubsetOf v u then GT' else NT'
    EQ -> if u == v then EQ' else NT'

instance PartialOrd IntSet where
  leq = IS.isSubsetOf

  compare' u v = case compare (IS.size u) (IS.size v) of
    LT -> if IS.isSubsetOf u v then LT' else NT'
    GT -> if IS.isSubsetOf u v then GT' else NT'
    EQ -> if u == v then EQ' else NT'


-- | Lists partially ordered by infix inclusion
newtype Infix a = Infix {
  unInfix :: [a]
} deriving (Eq, Show)

instance Eq a => PartialOrd (Infix a) where
  Infix a `leq` Infix b = isInfixOf a b

-- | Lists partially ordered by prefix inclusion
newtype Prefix a = Prefix {
  unPrefix :: [a]
} deriving (Eq, Show)

instance Eq a => PartialOrd (Prefix a) where
  compare' (Prefix a) (Prefix b) = let
    inner [] [] = EQ'
    inner [] _ = LT'
    inner _ [] = GT'
    inner (x:xs) (y:ys)
      | x == y    = inner xs ys
      | otherwise = NT'
    in inner a b
  Prefix a `leq` Prefix b = isPrefixOf a b


-- | Lists partially ordered by suffix inclusion
newtype Suffix a = Suffix {
  unSuffix :: [a]
} deriving (Eq, Show)

instance Eq a => PartialOrd (Suffix a) where
  Suffix a `leq` Suffix b = isSuffixOf a b


-- | Lists partially ordered by the subsequence relation
newtype Subseq a = Subseq {
  unSubseq :: [a]
} deriving (Eq, Show)

instance Eq a => PartialOrd (Subseq a) where
  Subseq a `leq` Subseq b = isSubsequenceOf a b


-- | Sets of incomparable elements, with a monoidal structure obtained
-- by taking the maximal ones.
--
-- Unfortunately, we need a full ordering for these to work (since
-- they use sets), though we don't assume this ordering has any
-- compatibility with the partial order. The monoid structures are
-- most efficient with pre-reduced sets as the left-hand argument.
newtype Maxima a = Maxima {
  maxSet :: Set a
}

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
newtype Minima a = Minima {
  minSet :: Set a
}

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
