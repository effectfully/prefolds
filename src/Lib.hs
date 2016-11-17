{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DefaultSignatures, BangPatterns #-}
module Lib
  ( module Lib
  , module Prelude
  , module Data.Functor.Identity
  , module Data.Bifunctor
  , module Control.Monad
  , module Control.Comonad
  , module Control.Monad.Morph
  ) where

import Prelude hiding (map, filter, takeWhile, dropWhile, span, take, drop, scan, groupBy, inits,
                       foldMap, length, sum, product)
import Data.Functor.Identity
import Data.Bifunctor
import Control.Monad (join, (>=>))
import Control.Comonad
import Control.Monad.Morph (MFunctor, hoist)

infixr 9 .*, <.>, <.*>
infixl 4 <+>, <$>>, <*>>, <+>>, +>
infixl 1 >>#
  
(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .* f = \x y -> g (f x y)
{-# INLINE (.*) #-}

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
g <.> f = fmap g . f
{-# INLINE (<.>) #-}

(<.*>) :: Functor f => (c -> d) -> (a -> b -> f c) -> a -> b -> f d
g <.*> f = fmap g .* f
{-# INLINE (<.*>) #-}

runEither :: Either a a -> a
runEither = either id id
{-# INLINE runEither #-}

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM f a xs = foldr (\x r (!a) -> f a x >>= r) return xs a
{-# INLINE foldM #-}

mfoldM :: (Foldable t, MonoMonad m) => (b -> a -> m b) -> b -> t a -> m b
mfoldM f a xs = foldr (\x r (!a) -> f a x >># r) mpure xs a
{-# INLINE mfoldM #-}

class Functor f => SumApplicative f where
  spure :: a -> f a
  (<+>) :: f (a -> b) -> f a -> f b

(+>) :: SumApplicative f => f a -> f b -> f b
a +> b = const id <$> a <+> b
{-# INLINE (+>) #-}

class SumApplicative m => SumMonad m where
  (>>+) :: m a -> (a -> m b) -> m b
  a >>+ f = sjoin $ fmap f a
  {-# INLINE (>>+) #-}

  sjoin :: m (m a) -> m a
  sjoin a = a >>+ id
  {-# INLINE sjoin #-}

class Functor m => MonoMonad m where
  mpure :: a -> m a
  default mpure :: Applicative m => a -> m a
  mpure = pure
  {-# INLINE mpure #-}

  (>>#) :: m a -> (a -> m a) -> m a

-- Transforms a Monad into a MonoMonad.
class MonoMonadTrans t where
  mlift :: Monad m => m a -> t m a

-- Transforms a Monad into a SumMonad.
class SumMonadTrans t where
  slift :: Monad m => m a -> t m a

-- A variant of http://elvishjerricco.github.io/2016/10/12/kleisli-functors.html
class (Monad m, Functor f) => KleisliFunctor m f where
  kmap :: (a -> m b) -> f a -> f b
  kmap = kjoin .* fmap
  {-# INLINE kmap #-}

  kjoin :: f (m a) -> f a
  kjoin = kmap id
  {-# INLINE kjoin #-}

(<$>>) :: KleisliFunctor m f => (a -> m b) -> f a -> f b
(<$>>) = kmap
{-# INLINE (<$>>) #-}

(<*>>) :: (KleisliFunctor m f, Applicative f) => f (a -> m b) -> f a -> f b
h <*>> a = kjoin $ h <*> a
{-# INLINE (<*>>) #-}

(<+>>) :: (KleisliFunctor m f, SumApplicative f) => f (a -> m b) -> f a -> f b
h <+>> a = kjoin $ h <+> a
{-# INLINE (<+>>) #-}

instance Monad m => KleisliFunctor m m where
  kmap = (=<<)
  {-# INLINE kmap #-}
