{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification #-}
module Fold where

import Lib hiding (foldM, mapAccumLM, scanM)
import qualified Lib
import qualified Pure

-- I'll probably change the definition to allow nested folds.
data Fold a m b = forall acc. Fold (Drive acc -> m b) (acc -> a -> DriveT m acc) (DriveT m acc)

driveFold :: Monad m => (b -> a -> DriveT m b) -> DriveT m b -> Fold a m b
driveFold = Fold (pure . runDrive)
{-# INLINEABLE driveFold #-}

drivePure :: Monad m => DriveT m b -> Fold a m b
drivePure = driveFold (pure .* const)
{-# INLINABLE drivePure #-}

driveMore :: Monad m => DriveT m b -> Fold a m b
driveMore = driveFold (more .* const)
{-# INLINABLE driveMore #-}

instance Monad m => Functor (Fold a m) where
  -- `fmap h . g` is not strictified, because I'm not sure it's needed.
  fmap h (Fold g f a) = Fold (h <.> g) f a
  {-# INLINEABLE fmap #-}

instance Monad m => Applicative (Fold a m) where
  pure = drivePure . pure
  {-# INLINABLE pure #-}

  -- `g` is not strictified for the same reason.
  Fold g1 f1 a1 <*> Fold g2 f2 a2 = Fold (final . runDrive) step (pairW a1 a2) where
    pairW a1 a2 = Pair <$> duplicate a1 <*> duplicate a2

    step (Pair a1 a2) x = pairW (a1 >># flip f1 x) (a2 >># flip f2 x)

    final (Pair a1 a2) = (getDriveT a1 >>= g1) <*> (getDriveT a2 >>= g2)
  {-# INLINABLE (<*>) #-}

instance MonadTrans (Fold a) where
  lift = drivePure . lift
  {-# INLINABLE lift #-}

instance MonadCall  (Fold a) where
  call = driveMore . call
  {-# INLINABLE call #-}

runFold :: Monad m => Fold a m b -> m b
runFold (Fold g f a) = getDriveT a >>= g
{-# INLINEABLE runFold #-}

feed :: Monad m => a -> Fold a m b -> DriveT m (Fold a m b)
feed x (Fold g f a) = extend (Fold g f) $ a >># flip f x
{-# INLINEABLE feed #-}

map :: Monad m => (b -> a) -> Fold a m c -> Fold b m c
map h (Fold g f a) = Fold g (\a -> f a . h) a
{-# INLINEABLE map #-}

filter :: Monad m => (a -> Bool) -> Fold a m b -> Fold a m b
filter p (Fold g f a) = Fold g (\a x -> if p x then f a x else more a) a
{-# INLINABLE filter #-}

-- The usual flaw: loses the first element for which the predicate doesn't hold.
takeWhile :: Monad m => (a -> Bool) -> Fold a m b -> Fold a m b
takeWhile p (Fold g f a) = Fold g (\a x -> if p x then f a x else pure a) a
{-# INLINABLE takeWhile #-}

dropWhile :: Monad m => (a -> Bool) -> Fold a m b -> Fold a m b
dropWhile p (Fold g f a) = Fold (g . fmap sndp) step (Pair False <$> a) where
  step (Pair b a) x | b || not (p x) = Pair True <$> f a x
                    | otherwise      = more $ Pair False a
{-# INLINABLE dropWhile #-}

take :: Monad m => Int -> Fold a m b -> Fold a m b
take n (Fold g f a) = Fold (g . fmap sndp) step acc where
  acc = stopWhen ((<= 0) . fstp) $ Pair n <$> a
  step (Pair n a) x | n <= 0    = error "prefolds.take: something went wrong"
                    | n == 1    = stop $ Pair 0 <$> f a x
                    | otherwise = Pair (n - 1) <$> f a x
{-# INLINABLE take #-}
  
drop :: Monad m => Int -> Fold a m b -> Fold a m b
drop n (Fold g f a) = Fold (g . fmap sndp) step (Pair n <$> a) where
  step (Pair n a) x | n <= 0    = Pair n <$> f a x
                    | otherwise = more $ Pair (n - 1) a
{-# INLINABLE drop #-}

stepFeed :: Monad m
         => DriveT m a
         -> (Drive a -> m b)
         -> DriveT m c
         -> (c -> b -> DriveT m c)
         -> DriveT m c
stepFeed a f b g = bindDriveT (getDriveT a >>= f) $ \y -> b >># flip g y
{-# INLINEABLE stepFeed #-}

finalFeed :: Monad m
          => DriveT m a
          -> (Drive a -> m b)
          -> DriveT m c
          -> (c -> b -> DriveT m c)
          -> (Drive c -> m d)
          -> m d
finalFeed a f b g h = getDriveT (stepFeed a f b g) >>= h
{-# INLINEABLE finalFeed #-}

scan :: Monad m => Fold a m b -> Fold b m c -> Fold a m c
scan (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold (final . runDrive) step (pairW a1 a2) where
  pairW a1 a2 = fromZipDriveT $ Pair <$> toZipDriveT (duplicate a1) <*> toZipDriveT (duplicate a2)

  step (Pair a1' a2') x = pairW (a1' >># flip f1 x) $ stepFeed a1' g1 a2' f2

  final (Pair a1' a2') = finalFeed a1' g1 a2' f2 g2
{-# INLINEABLE scan #-}

groupBy :: Monad m => (a -> a -> Bool) -> Fold a m b -> Fold b m c -> Fold a m c
groupBy p (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold (final . runDrive) step acc where
  acc = more . Pair (const True) $ Pair a1 a2

  step  (Pair p' (Pair a1' a2')) x
    | p' x      = more  . Pair (p x) $ Pair (a1' >># flip f1 x) a2'
    | otherwise = extend (Pair (p x) . Pair (a1  >># flip f1 x)) $ stepFeed a1' g1 a2' f2

  final (Pair p' (Pair a1' a2')) = finalFeed a1' g1 a2' f2 g2
{-# INLINABLE groupBy #-}

group :: (Monad m, Eq a) => Fold a m b -> Fold b m c -> Fold a m c
group = groupBy (==)
{-# INLINEABLE group #-}

consume :: (Monad m, Foldable t) => Fold a m b -> t a -> DriveT m (Fold a m b)
consume = Lib.foldM (flip feed)
{-# INLINABLE consume #-}

executeM :: (Monad m, Foldable t) => Fold a m b -> t a -> m b
executeM f = runDriveT . consume f >=> runFold
{-# INLINABLE executeM #-}

execute :: Foldable t => Fold a Identity b -> t a -> b
execute = runIdentity .* executeM 
{-# INLINABLE execute #-}

generalize :: Monad m => Pure.Fold a b -> Fold a m b
generalize (Pure.Fold g f a) = Fold (pure . g . runDrive) (more .* f) (more a)
{-# INLINABLE generalize #-}

foldM :: Monad m => (b -> a -> m b) -> b -> Fold a m b
foldM f = driveFold (call .* f) . more
{-# INLINABLE foldM #-}

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> Fold a m b
foldMapM f = foldM (\a x -> mappend a <$> f x) mempty
{-# INLINABLE foldMapM #-}

traverse_ :: Monad m => (a -> m ()) -> Fold a m ()
traverse_ f = foldM (const f) ()
{-# INLINABLE traverse_ #-}

fold  :: Monad m => (b -> a -> b) -> b -> Fold a m b
fold = generalize .* Pure.fold
{-# INLINABLE fold #-}

list :: Monad m => Fold a m [a]
list = generalize Pure.list
{-# INLINABLE list #-}

revList :: Monad m => Fold a m [a]
revList = generalize Pure.revList
{-# INLINABLE revList #-}

foldMap :: (Monad m, Monoid b) => (a -> b) -> Fold a m b
foldMap = generalize . Pure.foldMap
{-# INLINABLE foldMap #-}

mconcat :: (Monad m, Monoid a) => Fold a m a
mconcat = generalize Pure.mconcat
{-# INLINABLE mconcat #-}

null :: (Monad m, Num a) => Fold a m Bool
null = generalize Pure.null
{-# INLINABLE null #-}

length :: Monad m => Fold a m Int
length = generalize Pure.length
{-# INLINABLE length #-}

and :: Monad m => Fold Bool m Bool
and = generalize Pure.and
{-# INLINEABLE and #-}

or :: Monad m => Fold Bool m Bool
or = generalize Pure.or
{-# INLINEABLE or #-}

all :: Monad m => (a -> Bool) -> Fold a m Bool
all = generalize . Pure.all
{-# INLINEABLE all #-}

any :: Monad m => (a -> Bool) -> Fold a m Bool
any = generalize . Pure.any
{-# INLINEABLE any #-}

sum :: (Monad m, Num a) => Fold a m a
sum = generalize Pure.sum
{-# INLINABLE sum #-}

product :: (Monad m, Num a) => Fold a m a
product = generalize Pure.product
{-# INLINABLE product #-}

elem :: (Monad m, Eq a) => a -> Fold a m Bool
elem = generalize . Pure.elem
{-# INLINABLE elem #-}

notElem :: (Monad m, Eq a) => a -> Fold a m Bool
notElem = generalize . Pure.notElem
{-# INLINABLE notElem #-}

genericLength :: (Monad m, Num b) => Fold a m b
genericLength = generalize Pure.genericLength
{-# INLINABLE genericLength #-}
