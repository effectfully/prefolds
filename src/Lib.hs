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
                       groupBy, foldMap, length, sum)
import Data.Functor.Identity
import Data.Foldable
import Data.Traversable
import Data.Bifunctor
import Control.Monad (join, (>=>))
import Control.Comonad
import Control.Monad.Trans.Class

infixr 9 .*, <.>, <.*>
infixl 4 <&>
  
(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .* f = \x y -> g (f x y)
{-# INLINE (.*) #-}

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
g <.> f = fmap g . f
{-# INLINE (<.>) #-}

(<.*>) :: Functor f => (c -> d) -> (a -> b -> f c) -> a -> b -> f d
g <.*> f = fmap g .* f
{-# INLINE (<.*>) #-}

(<&>) :: (Monad m) => m (a -> m b) -> m a -> m b
f <&> a = f >>= (a >>=)
{-# INLINE (<&>) #-}

foldM :: (Foldable t, More m) => (b -> a -> m b) -> b -> t a -> m b
foldM f a xs = foldr (\x r (!a) -> f a x >># r) more xs a
{-# INLINABLE foldM #-}

data Pair a b = Pair !a !b

pattern Pair3 x1 x2 x3    = Pair x1 (Pair x2  x3)
pattern Pair4 x1 x2 x3 x4 = Pair x1 (Pair x2 (Pair x3 x4))

instance Functor (Pair a) where
  fmap g (Pair x y) = Pair x (g y)
  {-# INLINEABLE fmap #-}

instance Bifunctor Pair where
  bimap f g (Pair x y) = Pair (f x) (g y)
  {-# INLINEABLE bimap #-}

fstp :: Pair a b -> a
fstp (Pair x y) = x
{-# INLINEABLE fstp #-}

sndp :: Pair a b -> b
sndp (Pair x y) = y
{-# INLINEABLE sndp #-}

-- `more` is emphatically not `pure`, so `More` is unrelated to `Applicative`.
-- A `More` must satisfy the usual monad laws and
-- `fmap f (more x) ~ more (f x)`
-- I feel like there should be a law that relates `(>>#)` and `fmap`, is it
-- `fmap g (a >># f) ~ a >># fmap g . f`
-- where `g :: a -> a`?
class Functor m => More m where
  infixl 1 >>#
  more  :: a -> m a
  (>>#) :: m a -> (a -> m a) -> m a

stop :: (Applicative m, More m) => m a -> m a
stop a = a >># pure
{-# INLINEABLE stop #-}

stopWhen :: (Applicative m, More m) => (a -> Bool) -> m a -> m a
stopWhen p a = a >># \x -> if p x then pure x else more x
{-# INLINEABLE stopWhen #-}

-- `call . more ~ more`
-- `call (a >># f) ~ call a >># call . f`
class MonadCall t where
  call :: Monad m => m a -> t m a

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
  {-# INLINABLE fmap #-}

instance Applicative Drive where
  pure = Stop
  {-# INLINEABLE pure #-}
  
  Stop f <*> Stop x = Stop $ f x
  f      <*> x      = More $ runDrive f (runDrive x)
  {-# INLINEABLE (<*>) #-} 

instance Foldable Drive where
  foldMap = foldMapDefault
  {-# INLINEABLE foldMap #-}

instance Traversable Drive where
  traverse f = drive (Stop <.> f) (More <.> f)
  {-# INLINEABLE traverse #-}

instance Monad Drive where  
  a >>= f = join $ fmap f a where
    join (Stop (Stop x)) = Stop x
    join  a              = More $ runDrive (runDrive a)
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

instance Functor f => Functor (DriveT f) where
  fmap f (DriveT a) = DriveT $ fmap (fmap f) a
  {-# INLINEABLE fmap #-}

instance Applicative f => Applicative (DriveT f) where
  pure = DriveT . pure . Stop
  {-# INLINEABLE pure #-}
  
  DriveT h <*> DriveT a = DriveT $ (<*>) <$> h <*> a
  {-# INLINEABLE (<*>) #-}

instance Monad m => More (DriveT m) where
  more = DriveT . pure . More
  {-# INLINEABLE more #-}
  
  a >># f = driveDriveT pure f a
  {-# INLINABLE (>>#) #-}

instance Monad m => Comonad (DriveT m) where
  extract  = error "there is no `extract` for `DriveT m` unless `m` is a comonad, \
                   \ but this is not needed for `extend`, which is more important than `extract`"

  extend f = driveDriveT (pure . f . pure) (more . f . more)
  {-# INLINABLE extend #-}

instance MonadTrans DriveT where
  lift a = DriveT $ Stop <$> a
  {-# INLINEABLE lift #-}

instance MonadCall  DriveT where
  call a = DriveT $ More <$> a
  {-# INLINEABLE call #-}

revert :: Monad m => DriveT m a -> DriveT m a
revert = driveDriveT more pure
{-# INLINEABLE revert #-}

newtype ZipDriveT m a = ZipDriveT (DriveT m a)
                      deriving (Functor, Applicative, More, MonadTrans)

instance Monad m => Comonad (ZipDriveT m) where
  extract  (ZipDriveT a) = extract a
  {-# INLINABLE extract #-}
  
  extend f (ZipDriveT a) = ZipDriveT $ extend (f . ZipDriveT) a
  {-# INLINABLE extend #-}

toZipDriveT :: Monad m => DriveT m a -> ZipDriveT m a
toZipDriveT = ZipDriveT . revert
{-# INLINABLE toZipDriveT #-}

fromZipDriveT :: Monad m => ZipDriveT m a -> DriveT m a
fromZipDriveT (ZipDriveT a) = revert a
{-# INLINABLE fromZipDriveT #-}
