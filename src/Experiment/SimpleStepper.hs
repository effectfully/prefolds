{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification, NoImplicitPrelude #-}
module SimpleStepper where

import Lib
import Data.Strict.Tuple
import Data.Strict.Drive

class (Functor f, Functor g, Monad m) => Pairing f g m where
  interpret :: (a -> b -> m c) -> f b -> g a -> m c

data Stepper f m b = forall acc. Stepper (acc -> m b) (acc -> f (DriveT m acc)) (DriveT m acc)

instance Monad m => Functor (Stepper f m) where
  fmap h (Stepper g f a) = Stepper (h <.> g) f a
  {-# INLINEABLE fmap #-}

instance Pairing f g m => Pairing (Stepper f m) (Stepper g m) m where
  interpret h (Stepper g2 f2 a2) (Stepper g1 f1 a1) = go a1 a2 where
    go a1 a2 = driveTM (\(Pair a1' a2') -> h <$> g1 a1' <*>> g2 a2')
                       (\(Pair a1' a2') -> interpret go (f2 a2') (f1 a1'))
                       (Pair <$> a1 <&> a2)
  {-# INLINEABLE interpret #-}

take :: (Functor f, Monad m) => Int -> Stepper f m b -> Stepper f m b
take n (Stepper g f a) = Stepper (g . sndp) step (finish n a) where
  finish n a = terminateWhen ((<= 0) . fstp) $ Pair n <$> a

  step (Pair n a) | n <= 0    = error "Stepper.take: panic"
                  | otherwise = finish (n - 1) <$> f a
{-# INLINABLE take #-}

instance Monad m => Pairing ((->) a) ((,) (m a)) m where
  interpret g f (a, b) = a >>= g b . f
  {-# INLINEABLE interpret #-}

type Fold   a   = Stepper ((->) a)
type Unfold a m = Stepper ((,) (m a)) m

test :: Monad m => Fold a m (b -> m c) -> Unfold a m b -> m c
test = interpret (flip id)
