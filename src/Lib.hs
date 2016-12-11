{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DefaultSignatures, BangPatterns #-}
module Lib
  ( module Lib
  , module Prelude
  , module Data.Functor.Identity
  , module Data.Bifunctor
  , module Data.Traversable
  , module Control.Applicative
  , module Control.Monad
  , module Control.Comonad
  , module Control.Monad.Morph
  , module Control.Monad.Trans.Reader
  , module Control.Monad.Trans.State.Strict
  , module Control.Monad.Trans.Except
  ) where

import Prelude hiding (map, filter, takeWhile, dropWhile, span, take, drop,
                       foldMap, mconcat, null, length, all, any, and, or, sum, product,
                       elem, notElem, head, last, minimum, maximum,
                       iterate, repeat, enumFrom, enumFromTo, enumFromThenTo)
import Data.Functor.Identity
import Data.Bifunctor
import Data.Traversable (foldMapDefault)
import Control.Applicative ((<**>))
import Control.Monad (join, (>=>), ap)
import Control.Comonad
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Trans.Reader (ReaderT(..), ask, local)
import Control.Monad.Trans.State.Strict (StateT(..), get, put, modify')
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, catchE)

infixr 9 .*, <.>, <.*>
infixl 4 <+>, +>, <+, <&>, &>, <&, <$>>, <*>>, <+>>, <&>>
infixl 1 >>#, >#>

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .* f = \x y -> g (f x y)
{-# INLINE (.*) #-}

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
g <.> f = \x -> g <$> f x
{-# INLINE (<.>) #-}

(<.) :: Functor f => c -> (a -> f b) -> a -> f c
y <. f = \x -> y <$ f x
{-# INLINE (<.) #-}

(<.*>) :: Functor f => (c -> d) -> (a -> b -> f c) -> a -> b -> f d
g <.*> f = fmap g .* f
{-# INLINE (<.*>) #-}

tupl :: Functor f => a -> f b -> f (a, b)
tupl = fmap . (,)
{-# INLINE tupl #-}

tupr :: Functor f => f a -> b -> f (a, b)
tupr = flip $ fmap . flip (,)
{-# INLINE tupr #-}

runEither :: Either a a -> a
runEither = either id id
{-# INLINE runEither #-}

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM f a xs = foldr (\x r (!a') -> f a' x >>= r) return xs a
{-# INLINE foldM #-}

mfoldM :: (Foldable t, MonoMonad m) => (b -> a -> m b) -> b -> t a -> m b
mfoldM f a xs = foldr (\x r (!a') -> f a' x >># r) mpure xs a
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
class Functor m => MonoMonad m where
  mpure :: a -> m a
  (>>#) :: m a -> (a -> m a) -> m a

  (>#>) :: (a -> m a) -> (a -> m a) -> a -> m a
  f >#> g = \x -> f x >># g
  {-# INLINE (>#>) #-}

-- Transforms a Monad into a  SumApplicative.
class SumApplicativeTrans t where
  slift :: Monad m => m a -> t m a

-- Transforms a Monad into an AndApplicative.
class AndApplicativeTrans t where
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

class TransTraversable t s where
  sequenceT :: Monad m => t (s m) a -> s (t m) a
