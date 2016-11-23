{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
module Data.Strict.Drive where

import Lib

import Data.Traversable (foldMapDefault)
import Control.Monad.Trans.Except (ExceptT(..))

infixl 1 >>~, >~>

class (Functor f, Functor g) => Absorb f g where
  (>>~) :: f a -> (a -> g b) -> g b
  a >>~ f = ajoin $ fmap f a
  {-# INLINE (>>~) #-}

  ajoin :: f (g a) -> g a
  ajoin a = a >>~ id
  {-# INLINE ajoin #-}

(>~>) :: Absorb f m => (a -> f b) -> (b -> m c) -> a -> m c
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

driveToEither :: Drive a -> Either a a
driveToEither = drive Left Right
{-# INLINABLE driveToEither #-}

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

instance MonoMonad Drive where  
  a >># f = drive Stop f a
  {-# INLINABLE (>>#) #-}

instance Monad Drive where  
  a >>= f = join $ fmap f a where
    join (More (More x)) = More x
    join  a              = Stop $ runDrive (runDrive a)
  {-# INLINABLE (>>=) #-}

instance SumMonad Drive where  
  a >>+ f = join $ fmap f a where
    join (Stop (Stop x)) = Stop x
    join  a              = More $ runDrive (runDrive a)
  {-# INLINABLE (>>+) #-}

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

driveToDriveT :: Applicative f => Drive a -> DriveT f a
driveToDriveT = DriveT . pure
{-# INLINEABLE driveToDriveT #-}

driveT :: Functor f => (a -> b) -> (a -> b) -> DriveT f a -> f b
driveT g f (DriveT a) = drive g f <$> a
{-# INLINEABLE driveT #-}

driveTM :: Monad m => (a -> m b) -> (a -> m b) -> DriveT m a -> m b
driveTM g f (DriveT a) = a >>= drive g f
{-# INLINEABLE driveTM #-}

runDriveT :: Functor f => DriveT f a -> f a
runDriveT (DriveT a) = runDrive <$> a
{-# INLINEABLE runDriveT #-}

bindDriveT :: Monad m => (a -> DriveT m b) -> (a -> DriveT m b) -> DriveT m a -> DriveT m b
bindDriveT f g (DriveT a) = a >>~ drive f g
{-# INLINABLE bindDriveT #-}

isStopT :: Functor f => DriveT f a -> f Bool
isStopT (DriveT a) = isStop <$> a
{-# INLINABLE isStopT #-}

isMoreT :: Functor f => DriveT f a -> f Bool
isMoreT (DriveT a) = isMore <$> a
{-# INLINABLE isMoreT #-}

driveToExceptT :: Monad m => DriveT m a -> ExceptT a m a
driveToExceptT (DriveT a) = ExceptT $ driveToEither <$> a
{-# INLINABLE driveToExceptT #-}

-- A few slightly asymmetric synonyms to make things readable.
halt :: SumApplicative f => a -> f a
halt = spure
{-# INLINEABLE halt #-}

more :: MonoMonad m => a -> m a
more = mpure
{-# INLINEABLE more #-}

haltWhen :: (SumApplicative m, MonoMonad m) => (a -> Bool) -> a -> m a
haltWhen p x = if p x then halt x else more x
{-# INLINEABLE haltWhen #-}

moreWhen :: (SumApplicative m, MonoMonad m) => (a -> Bool) -> a -> m a
moreWhen p x = if p x then more x else halt x
{-# INLINEABLE moreWhen #-}

stop :: (SumMonadTrans t, Monad m) => m a -> t m a
stop = slift
{-# INLINEABLE stop #-}

keep :: (MonoMonadTrans t, Monad m) => m a -> t m a
keep = mlift
{-# INLINEABLE keep #-}

terminate :: (SumApplicative m, MonoMonad m) => m a -> m a
terminate a = a >># halt
{-# INLINEABLE terminate #-}

terminateWhen :: (SumApplicative m, MonoMonad m) => (a -> Bool) -> m a -> m a
terminateWhen p a = a >># \x -> if p x then halt x else more x
{-# INLINEABLE terminateWhen #-}

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
  a >># f = bindDriveT halt f a
  {-# INLINABLE (>>#) #-}

instance Monad m => Monad (DriveT m) where
  DriveT a >>= f = DriveT . fmap  join . join $ traverse (getDriveT . f) <$> a
  {-# INLINABLE (>>=) #-}

instance Monad m => SumMonad (DriveT m) where
  DriveT a >>+ f = DriveT . fmap sjoin . join $ traverse (getDriveT . f) <$> a
  {-# INLINABLE (>>+) #-}

instance Monad m => Comonad (DriveT m) where
  extract  = error "there is no `extract` for `DriveT m` unless `m` is a comonad, \
                   \ but this is not needed for `extend`, which is more important than `extract`"

  extend f = bindDriveT (halt . f . halt) (more . f . more)
  {-# INLINABLE extend #-}

instance MonoMonadTrans DriveT where
  mlift a = DriveT $ More <$> a
  {-# INLINEABLE mlift #-}

instance SumMonadTrans  DriveT where
  slift a = DriveT $ Stop <$> a
  {-# INLINEABLE slift #-}

instance MFunctor DriveT where
  hoist h (DriveT a) = DriveT $ h a
  {-# INLINEABLE hoist#-}

instance Monad m => Absorb (DriveT m) m where
  a >>~ f = runDriveT a >>= f
  {-# INLINEABLE (>>~) #-}

-- Is this law-abiding?
instance Monad m => Absorb m (DriveT m) where
  a >>~ f = DriveT $ a >>= getDriveT . f
  {-# INLINEABLE (>>~) #-}
