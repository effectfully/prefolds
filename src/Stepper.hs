{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification, NoImplicitPrelude #-}
module Core where

import Lib
import Data.Strict.Tuple
import Data.Strict.Drive

class (Functor f, Functor g, Monad m) => DualM f g m where
  zapWithM :: (a -> b -> m c) -> f a -> g b -> m c
  zapWithM h = zapM . fmap h
  {-# INLINE zapWithM #-}

  zapM :: f (a -> m b) -> g a -> m b
  zapM = zapWithM id
  {-# INLINE zapM #-}

data Stepper f m b = forall acc. Stepper (acc -> m b) (acc -> f (DriveT m acc)) (DriveT m acc)

instance (Functor f, Monad m) => Functor (Stepper f m) where
  fmap h (Stepper g f a) = Stepper (h <.> g) f a
  {-# INLINEABLE fmap #-}

instance DualM f g m => DualM (Stepper f m) (Stepper g m) m where
  zapWithM h (Stepper g2 f2 a2) (Stepper g1 f1 a1) = go a2 a1 where
    go a2 a1 = driveTM (\(Pair a2' a1') -> h <$> g2 a2' <*>> g1 a1')
                       (\(Pair a2' a1') -> zapWithM go (f2 a2') (f1 a1'))
                       (Pair <$> a2 <&> a1)
  {-# INLINEABLE zapWithM #-}

instance Monad m => DualM ((->) a) ((,) a) m where
  zapWithM g f = uncurry $ g . f
  {-# INLINEABLE zapWithM #-}

type Fold   a = Stepper ((->) a)
type Unfold a = Stepper ((,)  a)
