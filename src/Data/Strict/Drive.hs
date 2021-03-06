{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.Strict.Drive where

import Lib

infixl 1 >>~, >~>

class (Functor f, Functor g) => Absorb f g where
  (>>~) :: f a -> (a -> g b) -> g b
  a >>~ f = abjoin $ fmap f a
  {-# INLINE (>>~) #-}

  abjoin :: f (g a) -> g a
  abjoin a = a >>~ id
  {-# INLINE abjoin #-}

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

instance SumApplicative Drive where
  spure = Stop
  {-# INLINEABLE spure #-}
  
  Stop f <+> Stop x = Stop $ f x
  f      <+> x      = More $ runDrive f (runDrive x)
  {-# INLINEABLE (<+>) #-}

instance AndApplicative Drive where
  apure = More
  {-# INLINEABLE apure #-}
  
  More f <&> More x = More $ f x
  f      <&> x      = Stop $ runDrive f (runDrive x)
  {-# INLINEABLE (<&>) #-}

instance Foldable Drive where
  foldMap = foldMapDefault
  {-# INLINEABLE foldMap #-}

instance Traversable Drive where
  traverse f = drive (Stop <.> f) (More <.> f)
  {-# INLINEABLE traverse #-}

instance MonoMonad Drive where
  mpure = apure
  {-# INLINABLE mpure #-}

  a >># f = drive Stop f a
  {-# INLINABLE (>>#) #-}

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

driveDriveT :: Monad m => (a -> DriveT m b) -> (a -> DriveT m b) -> DriveT m a -> DriveT m b
driveDriveT f g (DriveT a) = a >>~ drive f g
{-# INLINABLE driveDriveT #-}

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

stop :: (SumApplicativeTrans t, Monad m) => m a -> t m a
stop = slift
{-# INLINEABLE stop #-}

keep :: (AndApplicativeTrans t, Monad m) => m a -> t m a
keep = alift
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

instance Applicative f => SumApplicative (DriveT f) where
  spure = DriveT . pure . Stop
  {-# INLINEABLE spure #-}

  DriveT h <+> DriveT a = DriveT $ (<+>) <$> h <*> a
  {-# INLINEABLE (<+>) #-}

instance Applicative f => AndApplicative (DriveT f) where
  apure = DriveT . pure . More
  {-# INLINEABLE apure #-}

  DriveT h <&> DriveT a = DriveT $ (<&>) <$> h <*> a
  {-# INLINEABLE (<&>) #-}

instance Foldable m => Foldable (DriveT m) where
  foldMap f (DriveT a) = foldMap (f . runDrive) a
  {-# INLINEABLE foldMap #-}

instance Traversable m => Traversable (DriveT m) where
  traverse f (DriveT a) = fmap DriveT $ traverse (traverse f) a
  {-# INLINEABLE traverse #-}

instance Monad m => MonoMonad (DriveT m) where
  mpure = apure
  {-# INLINABLE mpure #-}

  a >># f = driveDriveT halt f a
  {-# INLINABLE (>>#) #-}

instance Monad m => Comonad (DriveT m) where
  extract  = error "there is no `extract` for `DriveT m` unless `m` is a comonad, \
                   \ but this is not needed for `extend`, which is more important than `extract`"

  extend f = driveDriveT (halt . f . halt) (more . f . more)
  {-# INLINABLE extend #-}

instance SumApplicativeTrans DriveT where
  slift a = DriveT $ Stop <$> a
  {-# INLINEABLE slift #-}

instance AndApplicativeTrans DriveT where
  alift a = DriveT $ More <$> a
  {-# INLINEABLE alift #-}

instance MFunctor DriveT where
  hoist h (DriveT a) = DriveT $ h a
  {-# INLINEABLE hoist #-}

instance TransTraversable DriveT (ReaderT r) where
  sequenceT (DriveT (ReaderT f)) = ReaderT $ DriveT . f
  {-# INLINEABLE sequenceT #-}

runDriveReaderT :: Monad m => r -> DriveT (ReaderT r m) a -> DriveT m a
runDriveReaderT r = flip runReaderT r . sequenceT
{-# INLINEABLE runDriveReaderT #-}

instance TransTraversable DriveT (StateT s) where
  sequenceT (DriveT (StateT f)) = StateT $ DriveT . uncurry tupr <.> f
  {-# INLINEABLE sequenceT #-}

runDriveStateT :: Monad m => s -> DriveT (StateT s m) a -> DriveT m (a, s)
runDriveStateT s = flip runStateT s . sequenceT
{-# INLINEABLE runDriveStateT #-}

instance TransTraversable DriveT (ExceptT e) where
  sequenceT (DriveT (ExceptT s)) = ExceptT . DriveT $ either (Stop . Left) (fmap Right) <$> s 
  {-# INLINEABLE sequenceT #-}

runDriveExceptT :: Monad m => DriveT (ExceptT e m) a -> DriveT m (Either e a)
runDriveExceptT = runExceptT . sequenceT
{-# INLINEABLE runDriveExceptT #-}

instance Monad m => Absorb (DriveT m) m where
  a >>~ f = runDriveT a >>= f
  {-# INLINEABLE (>>~) #-}

instance Monad m => Absorb m (DriveT m) where
  a >>~ f = DriveT $ a >>= getDriveT . f
  {-# INLINEABLE (>>~) #-}
