{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms, LambdaCase, BangPatterns, GeneralizedNewtypeDeriving #-}
module Lib
    ( module Lib
    , module Prelude
    , module Data.Functor.Identity
    , module Control.Monad
    , module Control.Comonad
    , module Control.Monad.Trans.Class
    ) where

import Prelude hiding (map, filter, takeWhile, dropWhile, span, take, drop, scan,
                       groupBy, foldMap, length, sum, product)
import Data.Functor.Identity
import Data.Foldable
import Data.Traversable
import Data.Bifunctor
import Control.Monad (join, (>=>))
import Control.Comonad
import Control.Monad.Trans.Class

infixr 9 .*, <.>, <.*>
infixl 4 <+>, <$>>, <*>>
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

foldM :: (Foldable t, MonoMonad m) => (b -> a -> m b) -> b -> t a -> m b
foldM f a xs = foldr (\x r (!a) -> f a x >># r) mpure xs a
{-# INLINABLE foldM #-}

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

instance Monad m => KleisliFunctor m m where
  kmap = (=<<)
  {-# INLINE kmap #-}

class SumApplicative f where
  spure :: a -> f a
  (<+>) :: f (a -> b) -> f a -> f b

class Functor m => MonoMonad m where
  mpure :: a -> m a
  
  default mpure :: Applicative m => a -> m a
  mpure = pure
  {-# INLINE mpure #-}

  (>>#) :: m a -> (a -> m a) -> m a

-- Transforms a monad into a monomonad.
class MonoMonadTrans t where
  mlift :: Monad m => m a -> t m a

data Drive a = Stop !a | More !a

drive :: (a -> b) -> (a -> b) -> Drive a -> b
drive g f (Stop x) = g x
drive g f (More x) = f x
{-# INLINABLE drive #-}

runDrive :: Drive a -> a
runDrive = drive id id
{-# INLINEABLE runDrive #-}

isStop :: Drive a -> Bool
isStop = drive (const True) (const False)
{-# INLINABLE isStop #-}

instance Functor Drive where
  fmap f = drive (Stop . f) (More . f)
  {-# INLINE fmap #-}

instance Applicative Drive where
  pure = More
  {-# INLINE pure #-}
  
  More f <*> More x = More $ f x
  f      <*> x      = Stop $ runDrive f (runDrive x)
  {-# INLINEABLE (<*>) #-} 

instance SumApplicative Drive where
  spure = Stop
  {-# INLINE spure #-}
  
  Stop f <+> Stop x = Stop $ f x
  f      <+> x      = More $ runDrive f (runDrive x)
  {-# INLINEABLE (<+>) #-} 

instance Foldable Drive where
  foldMap = foldMapDefault
  {-# INLINEABLE foldMap #-}

instance Traversable Drive where
  traverse f = drive (Stop <.> f) (More <.> f)
  {-# INLINEABLE traverse #-}

instance Monad Drive where  
  a >>= f = join $ fmap f a where
    join (More (More x)) = More x
    join  a              = Stop $ runDrive (runDrive a)
  {-# INLINABLE (>>=) #-}

instance Comonad Drive where
  extract = runDrive
  {-# INLINEABLE extract #-}
  
  extend f = drive (Stop . f . Stop) (More . f . More)
  {-# INLINEABLE extend #-}

-- Is there a type class for this?
sequenceBi :: Bifunctor f => Drive (f a b) -> f (Drive a) (Drive b)
sequenceBi = drive (bimap Stop Stop) (bimap More More)
{-# INLINABLE sequenceBi #-}

newtype DriveT m a = DriveT { getDriveT :: m (Drive a) }

driveT :: Functor f => (a -> b) -> (a -> b) -> DriveT f a -> f b
driveT g f (DriveT a) = drive g f <$> a
{-# INLINEABLE driveT #-}

runDriveT :: Functor f => DriveT f a -> f a
runDriveT (DriveT a) = runDrive <$> a
{-# INLINEABLE runDriveT #-}

bindDriveT :: Monad m => m a -> (a -> DriveT m b) -> DriveT m b
bindDriveT a f = DriveT $ a >>= getDriveT . f
{-# INLINABLE bindDriveT #-}

driveDriveT :: Monad m => (a -> DriveT m b) -> (a -> DriveT m b) -> DriveT m a -> DriveT m b
driveDriveT f g (DriveT a) = bindDriveT a $ drive f g
{-# INLINABLE driveDriveT #-}

isStopT :: Functor f => DriveT f a -> f Bool
isStopT (DriveT a) = isStop <$> a
{-# INLINABLE isStopT #-}

-- A few specific synonyms to make things readable.
halt :: SumApplicative f => a -> f a
halt = spure
{-# INLINE halt #-}

more :: MonoMonad m => a -> m a
more = mpure
{-# INLINE more #-}

stop :: (MonoMonadTrans t, Monad m) => m a -> t m a
stop = mlift
{-# INLINE stop #-}

keep :: (MonadTrans t, Monad m) => m a -> t m a
keep = lift
{-# INLINE keep #-}

finish :: (SumApplicative m, MonoMonad m) => m a -> m a
finish a = a >># halt
{-# INLINEABLE finish #-}

finishWhen :: (SumApplicative m, MonoMonad m) => (a -> Bool) -> m a -> m a
finishWhen p a = a >># \x -> if p x then halt x else more x
{-# INLINEABLE finishWhen #-}

instance Functor f => Functor (DriveT f) where
  fmap f (DriveT a) = DriveT $ fmap (fmap f) a
  {-# INLINEABLE fmap #-}

instance Applicative f => Applicative (DriveT f) where
  pure = DriveT . pure . More
  {-# INLINEABLE pure #-}
  
  DriveT h <*> DriveT a = DriveT $ (<*>) <$> h <*> a
  {-# INLINEABLE (<*>) #-}

instance Applicative f => SumApplicative (DriveT f) where
  spure = DriveT . pure . Stop
  {-# INLINEABLE spure #-}
  
  DriveT h <+> DriveT a = DriveT $ (<+>) <$> h <*> a
  {-# INLINEABLE (<+>) #-}

instance Monad m => MonoMonad (DriveT m) where
  a >># f = driveDriveT halt f a
  {-# INLINABLE (>>#) #-}

instance Monad m => Comonad (DriveT m) where
  extract  = error "there is no `extract` for `DriveT m` unless `m` is a comonad, \
                   \ but this is not needed for `extend`, which is more important than `extract`"

  extend f = driveDriveT (halt . f . halt) (more . f . more)
  {-# INLINABLE extend #-}

instance MonadTrans     DriveT where
  lift  a = DriveT $ More <$> a
  {-# INLINEABLE lift #-}

instance MonoMonadTrans DriveT where
  mlift a = DriveT $ Stop <$> a
  {-# INLINEABLE mlift #-}
