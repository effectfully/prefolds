{-# LANGUAGE ExistentialQuantification #-}
module Pure where

import Prelude hiding (foldMap, all, any)

data Fold a b = forall acc. Fold (acc -> b) (acc -> a -> acc) acc

fold :: (b -> a -> b) -> b -> Fold a b
fold = Fold id
{-# INLINABLE fold #-}

list :: Fold a [a]
list = Fold ($ []) (\r x -> r . (x:)) id
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

null :: Num a => Fold a Bool
null = fold (\_ _ -> False) True
{-# INLINABLE null #-}

length :: Fold a Int
length = genericLength
{-# INLINABLE length #-}

and :: Fold Bool Bool
and = fold (&&) True
{-# INLINEABLE and #-}

or :: Fold Bool Bool
or = fold (||) False
{-# INLINEABLE or #-}

all :: (a -> Bool) -> Fold a Bool
all p = fold (\a x -> a && p x) True
{-# INLINEABLE all #-}

any :: (a -> Bool) -> Fold a Bool
any p = fold (\a x -> a || p x) False
{-# INLINEABLE any #-}

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
