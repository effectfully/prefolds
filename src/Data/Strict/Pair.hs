{-# LANGUAGE PatternSynonyms #-}
module Data.Strict.Pair where

import Data.Bifunctor

data Pair a b = Pair !a !b

pattern Pair3 x1 x2 x3       = Pair x1 (Pair x2  x3)
pattern Pair4 x1 x2 x3 x4    = Pair x1 (Pair x2 (Pair x3  x4))
pattern Pair5 x1 x2 x3 x4 x5 = Pair x1 (Pair x2 (Pair x3 (Pair x4 x5)))

instance Functor (Pair a) where
  fmap g (Pair x y) = Pair x (g y)
  {-# INLINE fmap #-}

instance Bifunctor Pair where
  bimap f g (Pair x y) = Pair (f x) (g y)
  {-# INLINE bimap #-}

fstp :: Pair a b -> a
fstp (Pair x y) = x
{-# INLINE fstp #-}

sndp :: Pair a b -> b
sndp (Pair x y) = y
{-# INLINE sndp #-}
