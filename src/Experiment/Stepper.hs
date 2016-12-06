{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification, NoImplicitPrelude #-}
module Stepper where

import Lib
import Data.Strict.Tuple
import Data.Strict.Drive

class (Functor (t m), Functor (s m), Monad m) => TransPairing t s m where
  interpretT :: (m a -> m b -> m c) -> t m b -> s m a -> m c

data Stepper t m b = forall acc. Stepper (acc -> m b) (acc -> DriveT (t m) acc) (DriveT m acc)

instance Monad m => Functor (Stepper t m) where
  fmap h (Stepper g f a) = Stepper (h <.> g) f a
  {-# INLINEABLE fmap #-}

interpretDriveT :: TransPairing t s m
                => (DriveT m a -> DriveT m b -> m c) -> DriveT (t m) b -> DriveT (s m) a -> m c
interpretDriveT f (DriveT b) (DriveT a) = interpretT (\a' b' -> f (DriveT a') (DriveT b')) b a
{-# INLINEABLE interpretDriveT #-}

instance TransPairing t s m => TransPairing (Stepper t) (Stepper s) m where
  interpretT h (Stepper g2 f2 a2) (Stepper g1 f1 a1) = go a1 a2 where
    go a1 a2 = driveTM (\(Pair a1' a2') -> h (g1 a1') (g2 a2'))
                       (\(Pair a1' a2') -> interpretDriveT go (f2 a2') (f1 a1'))
                       (Pair <$> a1 <&> a2)
  {-# INLINEABLE interpretT #-}

newtype Fun a m b = Fun { getFun :: a -> m b }
newtype Tup a m b = Tup { getTup :: m (Pair a b) }

instance Functor f => Functor (Fun a f) where
  fmap g (Fun f) = Fun $ g <.> f
  {-# INLINEABLE fmap #-}

instance Functor m => Functor (Tup a m) where
  fmap g (Tup p) = Tup $ (\(Pair x y) -> Pair x $ g y) <$> p
  {-# INLINEABLE fmap #-}

instance Monad m => TransPairing (Fun a) (Tup a) m where
  interpretT g (Fun f) (Tup p) = p >>= \(Pair x y) -> g (return y) (f x)
  {-# INLINEABLE interpretT #-}

--   DriveT (Fun a m) acc
-- ~ Fun a m (Drive acc)
-- ~ a -> m (Drive acc)
type Fold   a = Stepper (Fun a)
--   DriveT (Tup a m) acc
-- ~ Tup a m (Drive acc)
-- ~ m (Pair a (Drive acc))
type Unfold a = Stepper (Tup a)

test :: Monad m => Fold a m b -> Unfold a m c -> m b
test = interpretT (*>)
