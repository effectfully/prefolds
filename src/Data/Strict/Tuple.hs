module Data.Strict.Tuple where

import Data.Bifunctor

data Pair      a b     = Pair      !a !b
data Triple    a b c   = Triple    !a !b !c
data Quadruple a b c d = Quadruple !a !b !c !d

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)
  {-# INLINE fmap #-}

instance Bifunctor Pair where
  bimap f g (Pair a b) = Pair (f a) (g b)
  {-# INLINE bimap #-}

instance Monoid a => Applicative (Pair a) where
  pure = Pair mempty
  {-# INLINE pure #-}

  Pair x1 g <*> Pair x2 y = Pair (x1 `mappend` x2) (g y)
  {-# INLINE (<*>) #-}

instance Functor (Triple a b) where
  fmap f (Triple a b c) = Triple a b (f c)
  {-# INLINE fmap #-}

instance Bifunctor (Triple a) where
  bimap f g (Triple a b c) = Triple a (f b) (g c)
  {-# INLINE bimap #-}

instance Functor (Quadruple a b c) where
  fmap f (Quadruple a b c d) = Quadruple a b c (f d)
  {-# INLINE fmap #-}

instance Bifunctor (Quadruple a b) where
  bimap f g (Quadruple a b c d) = Quadruple a b (f c) (g d)
  {-# INLINE bimap #-}

fstp :: Pair a b -> a
fstp (Pair a b) = a
{-# INLINE fstp #-}

sndp :: Pair a b -> b
sndp (Pair a b) = b
{-# INLINE sndp #-}

fromPair :: Pair a b -> (a, b)
fromPair (Pair a b) = (a, b)
{-# INLINE fromPair #-}

toPair :: (a, b) -> Pair a b
toPair (a, b) = Pair a b
{-# INLINE toPair #-}

fstt :: Triple a b c -> a
fstt (Triple a b c) = a
{-# INLINE fstt #-}

sndt :: Triple a b c -> b
sndt (Triple a b c) = b
{-# INLINE sndt #-}

thdt :: Triple a b c -> c
thdt (Triple a b c) = c
{-# INLINE thdt #-}

fstq :: Quadruple a b c d -> a
fstq (Quadruple a b c d) = a
{-# INLINE fstq #-}

sndq :: Quadruple a b c d -> b
sndq (Quadruple a b c d) = b
{-# INLINE sndq #-}

thdq :: Quadruple a b c d -> c
thdq (Quadruple a b c d) = c
{-# INLINE thdq #-}

fthq :: Quadruple a b c d -> d
fthq (Quadruple a b c d) = d
{-# INLINE fthq #-}
