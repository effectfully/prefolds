{-# LANGUAGE NoImplicitPrelude #-}
module Fold where

import Lib hiding (foldM)
import Data.Strict.Drive
import qualified Pure
import Core

foldM :: Monad m => (b -> a -> m b) -> b -> Fold a m b
foldM f = driveFold (keep .* f) . more
{-# INLINABLE foldM #-}

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> Fold a m b
foldMapM f = foldM (\a x -> mappend a <$> f x) mempty
{-# INLINABLE foldMapM #-}

traverse_ :: Monad m => (a -> m ()) -> Fold a m ()
traverse_ f = foldM (const f) ()
{-# INLINABLE traverse_ #-}

fromPure :: Monad m => Pure.Fold a b -> Fold a m b
fromPure (Pure.Fold g f a) = Fold (pure . g) (driveToDriveT .* f) (more a)
{-# INLINABLE fromPure #-}

fold  :: Monad m => (b -> a -> b) -> b -> Fold a m b
fold = fromPure .* Pure.fold
{-# INLINABLE fold #-}

list :: Monad m => Fold a m [a]
list = fromPure Pure.list
{-# INLINABLE list #-}

revList :: Monad m => Fold a m [a]
revList = fromPure Pure.revList
{-# INLINABLE revList #-}

foldMap :: (Monad m, Monoid b) => (a -> b) -> Fold a m b
foldMap = fromPure . Pure.foldMap
{-# INLINABLE foldMap #-}

mconcat :: (Monad m, Monoid a) => Fold a m a
mconcat = fromPure Pure.mconcat
{-# INLINABLE mconcat #-}

null :: Monad m => Fold a m Bool
null = fromPure Pure.null
{-# INLINABLE null #-}

length :: Monad m => Fold a m Int
length = fromPure Pure.length
{-# INLINABLE length #-}

all :: Monad m => (a -> Bool) -> Fold a m Bool
all = fromPure . Pure.all
{-# INLINEABLE all #-}

any :: Monad m => (a -> Bool) -> Fold a m Bool
any = fromPure . Pure.any
{-# INLINEABLE any #-}

and :: Monad m => Fold Bool m Bool
and = fromPure Pure.and
{-# INLINEABLE and #-}

or :: Monad m => Fold Bool m Bool
or = fromPure Pure.or
{-# INLINEABLE or #-}

sum :: (Monad m, Num a) => Fold a m a
sum = fromPure Pure.sum
{-# INLINABLE sum #-}

product :: (Monad m, Num a) => Fold a m a
product = fromPure Pure.product
{-# INLINABLE product #-}

elem :: (Monad m, Eq a) => a -> Fold a m Bool
elem = fromPure . Pure.elem
{-# INLINABLE elem #-}

notElem :: (Monad m, Eq a) => a -> Fold a m Bool
notElem = fromPure . Pure.notElem
{-# INLINABLE notElem #-}

genericLength :: (Monad m, Num b) => Fold a m b
genericLength = fromPure Pure.genericLength
{-# INLINABLE genericLength #-}

head :: Monad m => Fold a m (Maybe a)
head = fromPure Pure.head
{-# INLINABLE head #-}

last :: Monad m => Fold a m (Maybe a)
last = fromPure Pure.last
{-# INLINABLE last #-}

find :: Monad m => (a -> Bool) -> Fold a m (Maybe a)
find = fromPure . Pure.find
{-# INLINABLE find #-}

minimum :: (Monad m, Ord a) => Fold a m (Maybe a)
minimum = fromPure Pure.minimum
{-# INLINABLE minimum #-}

maximum :: (Monad m, Ord a) => Fold a m (Maybe a)
maximum = fromPure Pure.maximum
{-# INLINABLE maximum #-}

minimumBy :: Monad m => (a -> a -> Ordering) -> Fold a m (Maybe a)
minimumBy = fromPure . Pure.minimumBy
{-# INLINABLE minimumBy #-}

maximumBy :: Monad m => (a -> a -> Ordering) -> Fold a m (Maybe a)
maximumBy = fromPure . Pure.maximumBy
{-# INLINABLE maximumBy #-}
