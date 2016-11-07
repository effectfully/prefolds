{-# LANGUAGE NoImplicitPrelude, RankNTypes, ExistentialQuantification #-}
module Fold where

import Lib hiding (foldM, mapAccumLM, scanM)
import qualified Lib
import qualified Pure

data Fold a m b = forall acc. Fold (acc -> m b) (acc -> a -> DriveT m acc) (DriveT m acc)

driveFold :: Monad m => (b -> a -> DriveT m b) -> DriveT m b -> Fold a m b
driveFold = Fold pure
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
  Fold g1 f1 a1 <*> Fold g2 f2 a2 = Fold final step (pairW a1 a2) where
    pairW a1 a2 = Pair <$> duplicate a1 <*> duplicate a2

    step (Pair a1' a2') x = pairW (a1' >># flip f1 x) (a2' >># flip f2 x)

    final (Pair a1' a2') = (runDriveT a1' >>= g1) <*> (runDriveT a2' >>= g2)
  {-# INLINABLE (<*>) #-}

instance MonadTrans (Fold a) where
  lift = drivePure . lift
  {-# INLINABLE lift #-}

instance MonadCall  (Fold a) where
  call = driveMore . call
  {-# INLINABLE call #-}

runFold :: Monad m => Fold a m b -> m b
runFold (Fold g f a) = runDriveT a >>= g
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
dropWhile p (Fold g f a) = Fold (g . sndp) step (Pair False <$> a) where
  step (Pair b a) x | b || not (p x) = Pair True <$> f a x
                    | otherwise      = more $ Pair False a
{-# INLINABLE dropWhile #-}

combine :: Monad m
        => (forall a. (a -> a) -> a -> a)
        -> (b -> c -> m d) -> Fold a m b -> Fold a m c -> Fold a m d
combine c h (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step acc where
  acc = bindDriveT (isStopT a1) $ \b -> Pair3 b <$> a1 <*> duplicate a2

  step (Pair3 b a1' a2') x
    | b         = extend (Pair3 True a1') $ a2' >># flip f2 x
    | otherwise = driveDriveT (\a1'' -> extend (Pair3 True  a1'') $ c (>># flip f2 x) a2')
                              (\a1'' -> more $ Pair3 False a1'' a2')
                              (f1 a1' x)

  final (Pair3 b a1' a2') = h <$> g1 a1' <&> (runDriveT a2' >>= g2)
{-# INLINABLE combine #-}

connectM :: Monad m => (b -> c -> m d) -> Fold a m b -> Fold a m c -> Fold a m d
connectM = combine (const id)
{-# INLINABLE connectM #-}

connect :: Monad m => (b -> c -> d) -> Fold a m b -> Fold a m c -> Fold a m d
connect f = connectM (pure .* f)
{-# INLINABLE connect #-}

connect_ :: Monad m => Fold a m b -> Fold a m c -> Fold a m c
connect_ = connect (const id)
{-# INLINABLE connect_ #-}

weldM :: Monad m => (b -> c -> m d) -> Fold a m b -> Fold a m c -> Fold a m d
weldM = combine ($)
{-# INLINABLE weldM #-}

weld :: Monad m => (b -> c -> d) -> Fold a m b -> Fold a m c -> Fold a m d
weld f = weldM (pure .* f)
{-# INLINABLE weld #-}

weld_ :: Monad m => Fold a m b -> Fold a m c -> Fold a m c
weld_ = weld (const id)
{-# INLINABLE weld_ #-}

spanM :: Monad m => (b -> c -> m d) -> (a -> Bool) -> Fold a m b -> Fold a m c -> Fold a m d
spanM h p = weldM h . takeWhile p
{-# INLINEABLE spanM #-}

span :: Monad m => (b -> c -> d) -> (a -> Bool) -> Fold a m b -> Fold a m c -> Fold a m d
span f = spanM (pure .* f)
{-# INLINEABLE span #-}

span_ :: Monad m => (a -> Bool) -> Fold a m b -> Fold a m c -> Fold a m c
span_ = span (const id)
{-# INLINEABLE span_ #-}

take :: Monad m => Int -> Fold a m b -> Fold a m b
take n (Fold g f a) = Fold (g . sndp) step acc where
  acc = stopWhen ((<= 0) . fstp) $ Pair n <$> a
  step (Pair n a) x | n <= 0    = error "prefolds.take: something went wrong"
                    | n == 1    = stop $ Pair 0 <$> f a x
                    | otherwise = Pair (n - 1) <$> f a x
{-# INLINABLE take #-}
  
drop :: Monad m => Int -> Fold a m b -> Fold a m b
drop n (Fold g f a) = Fold (g . sndp) step (Pair n <$> a) where
  step (Pair n a) x | n <= 0    = Pair n <$> f a x
                    | otherwise = more $ Pair (n - 1) a
{-# INLINABLE drop #-}

scan :: Monad m => Fold a m b -> Fold b m c -> Fold a m c
scan (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step (pair a1 a2) where
  pair a1 a2 = fromZipDriveT $ Pair <$> toZipDriveT a1 <*> toZipDriveT (duplicate a2)
  cross a1' a2' = bindDriveT (g1 a1') $ \y -> a2' >># flip f2 y

  step (Pair a1' a2') x = pair (f1 a1' x) $ cross a1' a2'

  final (Pair a1' a2') = runDriveT (cross a1' a2') >>= g2
{-# INLINEABLE scan #-}

groupBy :: Monad m => (a -> a -> Bool) -> Fold a m b -> Fold b m c -> Fold a m c
groupBy p (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step acc where
  cross a1' a2' = bindDriveT (runDriveT a1' >>= g1) (f2 a2')
  
  acc = Pair3 (const True) a1 <$> a2

  step (Pair3 p' a1' a2') x
    | p' x      = more $ pair a1' a2'
    | otherwise = pair a1 <$> cross a1' a2'
    where pair a = Pair3 (p x) (a >># flip f1 x)

  final (Pair3 p' a1' a2') = runDriveT (cross a1' a2') >>= g2
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
generalize (Pure.Fold g f a) = Fold (pure . g) (more .* f) (more a)
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
