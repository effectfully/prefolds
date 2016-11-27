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
                       foldMap, mconcat, null, length, all, any, and, or, sum, product,
                       genericLength, head, last, minimum, maximum, minimumBy, maximumBy)
import Data.Functor.Identity
import Data.Bifunctor
import Control.Monad (join, (>=>))
import Control.Comonad
import Control.Monad.Morph (MFunctor, hoist)

infixr 9 .*, <.>, <.*>
infixl 4 <+>, +>, <+, <&>, &>, <&, <$>>, <*>>, <+>>, <&>>
infixl 1 >>+, >+>, >>&, >&>, >>#, >#>

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

mfoldM :: (Foldable t, AndMonoMonad m) => (b -> a -> m b) -> b -> t a -> m b
mfoldM f a xs = foldr (\x r (!a) -> f a x >># r) ampure xs a
{-# INLINE mfoldM #-}

-- The usual Applicative laws.
class Functor f => SumApplicative f where
  spure :: a -> f a
  (<+>) :: f (a -> b) -> f a -> f b

  (+>) :: f a -> f b -> f b
  a +> b = id <$ a <+> b
  {-# INLINE (+>) #-}

  (<+) :: f a -> f b -> f a
  a <+ b = const <$> a <+> b
  {-# INLINE (<+) #-}

-- The usual Applicative laws.
class Functor f => AndApplicative f where
  apure :: a -> f a
  (<&>) :: f (a -> b) -> f a -> f b

  (&>) :: f a -> f b -> f b
  a &> b = id <$ a <&> b
  {-# INLINE (&>) #-}

  (<&) :: f a -> f b -> f a
  a <& b = const <$> a <&> b
  {-# INLINE (<&) #-}

-- The usual Monad laws.
class SumApplicative m => SumMonad m where
  (>>+) :: m a -> (a -> m b) -> m b
  a >>+ f = sjoin $ fmap f a
  {-# INLINE (>>+) #-}

  sjoin :: m (m a) -> m a
  sjoin a = a >>+ id
  {-# INLINE sjoin #-}

  (>+>) :: (a -> m b) -> (b -> m c) -> a -> m c
  f >+> g = \x -> f x >>+ g
  {-# INLINE (>+>) #-}

-- The usual Monad laws.
class AndApplicative m => AndMonad m where
  (>>&) :: m a -> (a -> m b) -> m b
  a >>& f = ajoin $ fmap f a
  {-# INLINE (>>&) #-}

  ajoin :: m (m a) -> m a
  ajoin a = a >>& id
  {-# INLINE ajoin #-}

  (>&>) :: (a -> m b) -> (b -> m c) -> a -> m c
  f >&> g = \x -> f x >>& g
  {-# INLINE (>&>) #-}

-- The usual Monad laws.
class Functor m => AndMonoMonad m where
  ampure :: a -> m a
  default ampure :: AndApplicative m => a -> m a
  ampure = apure
  {-# INLINE ampure #-}

  (>>#) :: m a -> (a -> m a) -> m a

  (>#>) :: (a -> m a) -> (a -> m a) -> a -> m a
  f >#> g = \x -> f x >># g
  {-# INLINE (>#>) #-}

-- Transforms a Monad into a SumMonad.
class SumMonadTrans t where
  slift :: Monad m => m a -> t m a

-- Transforms a Monad into a AndMonad.
class AndMonadTrans t where
  alift :: Monad m => m a -> t m a

-- A variant of http://elvishjerricco.github.io/2016/10/12/kleisli-functors.html
-- kmap return     === id
-- kmap g . kmap f === kmap (f >=> g)
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

(<&>>) :: (KleisliFunctor m f, AndApplicative f) => f (a -> m b) -> f a -> f b
h <&>> a = kjoin $ h <&> a
{-# INLINE (<&>>) #-}

instance Monad m => KleisliFunctor m m where
  kmap = (=<<)
  {-# INLINE kmap #-}
