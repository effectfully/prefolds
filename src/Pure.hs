{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}
module Pure where

import Lib
import Data.Strict.Maybe
import Data.Strict.Drive

data Fold a b = forall acc. Fold (acc -> b) (acc -> a -> Drive acc) acc

foldDrive :: (b -> a -> Drive b) -> b -> Fold a b
foldDrive = Fold id
{-# INLINEABLE foldDrive #-}

fold1Drive :: (a -> a -> Drive a) -> Fold a (Maybe a)
fold1Drive f = Fold lazy (\ma x -> Just' <$> maybe' (more x) (flip f x) ma) Nothing'
{-# INLINEABLE fold1Drive #-}

fold :: (b -> a -> b) -> b -> Fold a b
fold f = foldDrive (more .* f)
{-# INLINABLE fold #-}

fold1 :: (a -> a -> a) -> Fold a (Maybe a)
fold1 f = fold1Drive (more .* f)
{-# INLINABLE fold1 #-}

list :: Fold a [a]
list = Fold ($ []) (\r x -> more $ r . (x:)) id
{-# INLINABLE list #-}

revList :: Fold a [a]
revList = fold (flip (:)) []
{-# INLINABLE revList #-}

foldMap :: Monoid b => (a -> b) -> Fold a b
foldMap f = fold (\a x -> a `mappend` f x) mempty
{-# INLINABLE foldMap #-}

mconcat :: Monoid a => Fold a a
mconcat = foldMap id
{-# INLINABLE mconcat #-}

null :: Fold a Bool
null = foldDrive (\_ _ -> halt False) True
{-# INLINABLE null #-}

length :: Fold a Int
length = genericLength
{-# INLINABLE length #-}

all :: (a -> Bool) -> Fold a Bool
all p = foldDrive (\a x -> moreWhen id $ a && p x) True
{-# INLINEABLE all #-}

any :: (a -> Bool) -> Fold a Bool
any p = foldDrive (\a x -> haltWhen id $ a || p x) False
{-# INLINEABLE any #-}

and :: Fold Bool Bool
and = all id
{-# INLINEABLE and #-}

or :: Fold Bool Bool
or = any id
{-# INLINEABLE or #-}

sum :: Num a => Fold a a
sum = fold (+) 0
{-# INLINABLE sum #-}

product :: Num a => Fold a a
product = fold (*) 1
{-# INLINABLE product #-}

elem :: Eq a => a -> Fold a Bool
elem a = any (a ==)
{-# INLINABLE elem #-}

notElem :: Eq a => a -> Fold a Bool
notElem a = all (a /=)
{-# INLINABLE notElem #-}

genericLength :: Num b => Fold a b
genericLength = fold (\a _ -> 1 + a) 0
{-# INLINABLE genericLength #-}

find :: (a -> Bool) -> Fold a (Maybe a)
find p = foldDrive f Nothing where
  f _ x = if p x then halt (Just x) else more Nothing
{-# INLINABLE find #-}

head :: Fold a (Maybe a)
head = foldDrive (\_ -> halt . Just) Nothing
{-# INLINABLE head #-}

last :: Fold a (Maybe a)
last = fold1 (const id)
{-# INLINABLE last #-}

minimum :: Ord a => Fold a (Maybe a)
minimum = fold1 min
{-# INLINABLE minimum #-}

maximum :: Ord a => Fold a (Maybe a)
maximum = fold1 max
{-# INLINABLE maximum #-}

minimumBy :: (a -> a -> Ordering) -> Fold a (Maybe a)
minimumBy c = fold1 f where
  f x y = case c x y of
    GT -> y
    _  -> x
{-# INLINABLE minimumBy #-}

maximumBy :: (a -> a -> Ordering) -> Fold a (Maybe a)
maximumBy c = fold1 f where
  f x y = case c x y of
      GT -> x
      _  -> y
{-# INLINABLE maximumBy #-}

