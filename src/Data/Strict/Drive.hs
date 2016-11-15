{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
module Data.Strict.Drive where

import Lib

import Data.Traversable (foldMapDefault)

infixl 1 >>~, >~>

class Absorb f g where
  (>>~) :: f a -> (a -> g b) -> g b

(>~>) :: Absorb f g => (a -> f b) -> (b -> g c) -> a -> g c
f >~> g = \x -> f x >>~ g
{-# INLINE (>~>) #-}

data Drive a = Stop !a | More !a

drive :: (a -> b) -> (a -> b) -> Drive a -> b
drive f g (Stop x) = f x
drive f g (More x) = g x
{-# INLINABLE drive #-}

runDrive :: Drive a -> a
runDrive = drive id id
{-# INLINEABLE runDrive #-}

isStop :: Drive a -> Bool
isStop = drive (const True) (const False)
{-# INLINABLE isStop #-}

isMore :: Drive a -> Bool
isMore = drive (const False) (const True)
{-# INLINABLE isMore #-}

instance Functor Drive where
  fmap f = drive (Stop . f) (More . f)
  {-# INLINEABLE fmap #-}

instance Applicative Drive where
  pure = More
  {-# INLINEABLE pure #-}
  
  More f <*> More x = More $ f x
  f      <*> x      = Stop $ runDrive f (runDrive x)
  {-# INLINEABLE (<*>) #-} 

instance SumApplicative Drive where
  spure = Stop
  {-# INLINEABLE spure #-}
  
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

driveDriveT :: Monad m => (a -> DriveT m b) -> (a -> DriveT m b) -> DriveT m a -> DriveT m b
driveDriveT f g (DriveT a) = a >>~ drive f g
{-# INLINABLE driveDriveT #-}

isStopT :: Functor f => DriveT f a -> f Bool
isStopT (DriveT a) = isStop <$> a
{-# INLINABLE isStopT #-}

isMoreT :: Functor f => DriveT f a -> f Bool
isMoreT (DriveT a) = isMore <$> a
{-# INLINABLE isMoreT #-}

-- A few synonyms to make things readable.
halt :: SumApplicative f => a -> f a
halt = spure
{-# INLINEABLE halt #-}

more :: MonoMonad m => a -> m a
more = mpure
{-# INLINEABLE more #-}

stop :: (MonoMonadTrans t, Monad m) => m a -> t m a
stop = mlift
{-# INLINEABLE stop #-}

keep :: (MonadTrans t, Monad m) => m a -> t m a
keep = lift
{-# INLINEABLE keep #-}

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

instance MFunctor DriveT where
  hoist h (DriveT a) = DriveT $ h a
  {-# INLINEABLE hoist#-}

instance Monad m => Absorb (DriveT m) m where
  a >>~ f = runDriveT a >>= f
  {-# INLINEABLE (>>~) #-}

instance Monad m => Absorb m (DriveT m) where
  a >>~ f = DriveT $ a >>= getDriveT . f
  {-# INLINEABLE (>>~) #-}
