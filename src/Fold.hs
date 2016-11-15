{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Fold where

import Lib hiding (foldM, mapAccumLM, scanM)
import Data.Strict.Pair
import Data.Strict.Drive
import qualified Lib
import qualified Pure

infixl 4 </>, />, </>>, <//>, //>, <//>>

data Fold a m b = forall acc. Fold (acc -> m b) (acc -> a -> DriveT m acc) (DriveT m acc)

driveFold :: Monad m => (b -> a -> DriveT m b) -> DriveT m b -> Fold a m b
driveFold = Fold pure
{-# INLINEABLE driveFold #-}

driveHalt :: Monad m => DriveT m b -> Fold a m b
driveHalt = driveFold (error "prefolds.driveHalt: something went wrong")
{-# INLINABLE driveHalt #-}

driveMore :: Monad m => DriveT m b -> Fold a m b
driveMore = driveFold (more .* const)
{-# INLINABLE driveMore #-}

instance Monad m => Functor (Fold a m) where
  -- `h <.> g` is not strictified, because I'm not sure it's needed.
  -- The same applies to other instances.
  fmap h (Fold g f a) = Fold (h <.> g) f a
  {-# INLINEABLE fmap #-}

instance Monad m => KleisliFunctor m (Fold a m) where
  kmap h (Fold g f a) = Fold (g >=> h) f a
  {-# INLINEABLE kmap #-}

instance Monad m => Applicative (Fold a m) where
  pure = driveMore . pure
  {-# INLINABLE pure #-}

  Fold g1 f1 a1 <*> Fold g2 f2 a2 = Fold final step (Pair <$> a1 <*> a2) where
    step (Pair a1' a2') x = Pair <$> f1 a1' x <*> f2 a2' x

    final (Pair a1' a2') = g1 a1' <*> g2 a2'
  {-# INLINABLE (<*>) #-}

instance Monad m => SumApplicative (Fold a m) where
  spure = driveHalt . spure
  {-# INLINABLE spure #-}

  Fold g1 f1 a1 <+> Fold g2 f2 a2 = Fold final step (pairW a1 a2) where
    pairW a1 a2 = Pair <$> duplicate a1 <+> duplicate a2

    step (Pair a1' a2') x = pairW (a1' >># flip f1 x) (a2' >># flip f2 x)

    final (Pair a1' a2') = (a1' >>~ g1) <*> (a2' >>~ g2)
  {-# INLINABLE (<+>) #-}

instance MonadTrans     (Fold a) where
  lift  = driveMore . lift
  {-# INLINABLE lift #-}

instance MonoMonadTrans (Fold a) where
  mlift = driveHalt . mlift
  {-# INLINABLE mlift #-}

instance MFunctor (Fold a) where
  hoist h (Fold g f a) = Fold (h . g) (hoist h .* f) (hoist h a)
  {-# INLINEABLE hoist #-}

runFold :: Monad m => Fold a m b -> m b
runFold (Fold g f a) = a >>~ g
{-# INLINEABLE runFold #-}

feed :: Monad m => a -> Fold a m b -> DriveT m (Fold a m b)
feed x (Fold g f a) = a >># flip f x =>> Fold g f
{-# INLINEABLE feed #-}

combine :: Monad m
        => (forall a. (a -> a) -> a -> a)
        -> Fold a m (b -> c) -> Fold a m b -> Fold a m c
combine c (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step acc where
  acc = isStopT a1 >>~ \b -> Pair3 b <$> a1 <+> duplicate a2

  step (Pair3 b a1' a2') x
    | b         = a2' >># flip f2 x =>> Pair3 True a1'
    | otherwise = driveDriveT (\a1'' -> c (>># flip f2 x) a2' =>> Pair3 True a1'')
                              (\a1'' -> more $ Pair3 False a1'' a2')
                              (f1 a1' x)

  final (Pair3 b a1' a2') = g1 a1' <*> (a2' >>~ g2)
{-# INLINABLE combine #-}

(</>) :: Monad m => Fold a m (b -> c) -> Fold a m b -> Fold a m c
(</>) = combine (const id)
{-# INLINEABLE (</>) #-}

(/>) :: Monad m => Fold a m b -> Fold a m c -> Fold a m c
b /> c = const id <$> b </> c
{-# INLINEABLE (/>) #-}

(</>>) :: Monad m => Fold a m (b -> m c) -> Fold a m b -> Fold a m c
(</>>) = kjoin .* (</>)
{-# INLINEABLE (</>>) #-}

(<//>) :: Monad m => Fold a m (b -> c) -> Fold a m b -> Fold a m c
(<//>) = combine id
{-# INLINEABLE (<//>) #-}

(//>) :: Monad m => Fold a m b -> Fold a m c -> Fold a m c
b //> c = const id <$> b <//> c
{-# INLINEABLE (//>) #-}

(<//>>) :: Monad m => Fold a m (b -> m c) -> Fold a m b -> Fold a m c
(<//>>) = kjoin .* (<//>)
{-# INLINEABLE (<//>>) #-}

map :: Monad m => (b -> a) -> Fold a m c -> Fold b m c
map h (Fold g f a) = Fold g (\a -> f a . h) a
{-# INLINEABLE map #-}

filter :: Monad m => (a -> Bool) -> Fold a m b -> Fold a m b
filter p (Fold g f a) = Fold g (\a x -> if p x then f a x else more a) a
{-# INLINABLE filter #-}

-- The usual flaw: loses the first element for which the predicate doesn't hold.
takeWhile :: Monad m => (a -> Bool) -> Fold a m b -> Fold a m b
takeWhile p (Fold g f a) = Fold g (\a x -> if p x then f a x else halt a) a
{-# INLINABLE takeWhile #-}

dropWhile :: Monad m => (a -> Bool) -> Fold a m b -> Fold a m b
dropWhile p (Fold g f a) = Fold (g . sndp) step (Pair False <$> a) where
  step (Pair b a) x | b || not (p x) = Pair True <$> f a x
                    | otherwise      = more $ Pair False a
{-# INLINABLE dropWhile #-}

spanM :: Monad m => (b -> c -> m d) -> (a -> Bool) -> Fold a m b -> Fold a m c -> Fold a m d
spanM h p b c = h <$> takeWhile p b <//>> c
{-# INLINEABLE spanM #-}

span :: Monad m => (b -> c -> d) -> (a -> Bool) -> Fold a m b -> Fold a m c -> Fold a m d
span f = spanM (pure .* f)
{-# INLINEABLE span #-}

span_ :: Monad m => (a -> Bool) -> Fold a m b -> Fold a m c -> Fold a m c
span_ = span (const id)
{-# INLINEABLE span_ #-}

take :: Monad m => Int -> Fold a m b -> Fold a m b
take n (Fold g f a) = Fold (g . sndp) step acc where
  acc = finishWhen ((<= 0) . fstp) $ Pair n <$> a
  step (Pair n a) x | n <= 0    = error "prefolds.take: something went wrong"
                    | n == 1    = finish $ Pair 0 <$> f a x
                    | otherwise = Pair (n - 1) <$> f a x
{-# INLINABLE take #-}
  
drop :: Monad m => Int -> Fold a m b -> Fold a m b
drop n (Fold g f a) = Fold (g . sndp) step (Pair n <$> a) where
  step (Pair n a) x | n <= 0    = Pair n <$> f a x
                    | otherwise = more $ Pair (n - 1) a
{-# INLINABLE drop #-}

cross :: Monad m => m a -> DriveT m b -> (b -> a -> DriveT m b) -> DriveT m b
cross a b f = a >>~ \x -> b >># flip f x
{-# INLINABLE cross #-}

-- | `execute (scan f g) xs` scans `xs` with `f`, then folds the resulting list with `g`.
scan :: Monad m => Fold a m b -> Fold b m c -> Fold a m c
scan (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step (pair a1 a2) where
  pair a1 a2 = Pair <$> a1 <*> duplicate a2

  step (Pair a1' a2') x = pair (f1 a1' x) $ cross (g1 a1') a2' f2

  final (Pair a1' a2') = cross (g1 a1') a2' f2 >>~ g2
{-# INLINEABLE scan #-}

-- | `execute (groupBy p f g) xs` groups elements of `xs` by `p`, then folds each sublist with `f`,
-- then folds the resulting list with `g`.
-- Unlike the prelude version, `p` must be only transitive and is not required to be symmetric.
groupBy :: Monad m => (a -> a -> Bool) -> Fold a m b -> Fold b m c -> Fold a m c
groupBy p (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step acc where
  acc = a2 =>> Pair4 False (const True) a1

  step (Pair4 _ p' a1' a2') x
    | p' x      = more $ pair a1' a2'
    | otherwise = cross (a1' >>~ g1) a2' f2 =>> pair a1
    where pair a = Pair4 True (p x) (a >># flip f1 x)

  final (Pair4 b _ a1' a2') = (if b then cross (a1' >>~ g1) a2' f2 else a2') >>~ g2
{-# INLINABLE groupBy #-}

-- | Same as `groupBy`, but is slightly more efficient.
-- The only difference is that this version emulates `Prelude.groupBy p [] = [[]]`.
groupBy1 :: Monad m => (a -> a -> Bool) -> Fold a m b -> Fold b m c -> Fold a m c
groupBy1 p (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step acc where
  acc = a2 =>> Pair3 (const True) a1

  step (Pair3 p' a1' a2') x | p' x      = more $ pair a1' a2'
                            | otherwise = cross (a1' >>~ g1) a2' f2 =>> pair a1
                            where pair a = Pair3 (p x) (a >># flip f1 x)

  final (Pair3 _ a1' a2') = cross (a1' >>~ g1) a2' f2 >>~ g2
{-# INLINABLE groupBy1 #-}

group :: (Monad m, Eq a) => Fold a m b -> Fold b m c -> Fold a m c
group = groupBy (==)
{-# INLINEABLE group #-}

-- | `execute (inits f g) xs` folds "inits" of `xs` with `f` and then folds
-- the resulting list with `g`.
inits :: Monad m => Fold a m b -> Fold b m c -> Fold a m c
inits (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step (a2 =>> Pair a1) where
  step (Pair a1' a2') x = cross (a1' >>~ g1) a2' f2 =>> Pair (a1' >># flip f1 x)

  final (Pair a1' a2') = cross (a1' >>~ g1) a2' f2 >>~ g2
{-# INLINABLE inits #-}

chunks :: Monad m => Fold a m b -> Fold b m c -> Fold a m c
chunks (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step (init a2) where
  init a2' = Pair3 False <$> a1 <*> duplicate a2'

  step (Pair3 _ a1' a2') x = driveDriveT (\a1'' -> init $ cross (g1 a1'') a2' f2)
                                         (\a1'' -> more $ Pair3 True a1'' a2')
                                         (f1 a1' x)

  final (Pair3 b a1' a2') = (if b then cross (g1 a1') a2' f2 else a2') >>~ g2
{-# INLINABLE chunks #-}

consume :: (Monad m, Foldable t) => Fold a m b -> t a -> DriveT m (Fold a m b)
consume = Lib.foldM (flip feed)
{-# INLINABLE consume #-}

executeM :: (Monad m, Foldable t) => Fold a m b -> t a -> m b
executeM f = runDriveT . consume f >=> runFold
{-# INLINABLE executeM #-}

execute :: Foldable t => Fold a Identity b -> t a -> b
execute = runIdentity .* executeM
{-# INLINABLE execute #-}

fromPure :: Monad m => Pure.Fold a b -> Fold a m b
fromPure (Pure.Fold g f a) = Fold (pure . g) (more .* f) (more a)
{-# INLINABLE fromPure #-}

foldM :: Monad m => (b -> a -> m b) -> b -> Fold a m b
foldM f = driveFold (keep .* f) . more
{-# INLINABLE foldM #-}

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> Fold a m b
foldMapM f = foldM (\a x -> mappend a <$> f x) mempty
{-# INLINABLE foldMapM #-}

traverse_ :: Monad m => (a -> m ()) -> Fold a m ()
traverse_ f = foldM (const f) ()
{-# INLINABLE traverse_ #-}

fold  :: Monad m => (b -> a -> b) -> b -> Fold a m b
fold = fromPure .* Pure.fold
{-# INLINABLE fold #-}

list :: Monad m => Fold a m [a]
list = fromPure Pure.list
{-# INLINABLE list #-}

revList :: Monad m => Fold a m [a]
revList = fromPure Pure.revList
{-# INLINABLE revList #-}

foldMap :: (Monad m, Monoid b) => (a -> b) -> Fold a m b
foldMap = fromPure . Pure.foldMap
{-# INLINABLE foldMap #-}

mconcat :: (Monad m, Monoid a) => Fold a m a
mconcat = fromPure Pure.mconcat
{-# INLINABLE mconcat #-}

null :: (Monad m, Num a) => Fold a m Bool
null = fromPure Pure.null
{-# INLINABLE null #-}

length :: Monad m => Fold a m Int
length = fromPure Pure.length
{-# INLINABLE length #-}

and :: Monad m => Fold Bool m Bool
and = fromPure Pure.and
{-# INLINEABLE and #-}

or :: Monad m => Fold Bool m Bool
or = fromPure Pure.or
{-# INLINEABLE or #-}

all :: Monad m => (a -> Bool) -> Fold a m Bool
all = fromPure . Pure.all
{-# INLINEABLE all #-}

any :: Monad m => (a -> Bool) -> Fold a m Bool
any = fromPure . Pure.any
{-# INLINEABLE any #-}

sum :: (Monad m, Num a) => Fold a m a
sum = fromPure Pure.sum
{-# INLINABLE sum #-}

product :: (Monad m, Num a) => Fold a m a
product = fromPure Pure.product
{-# INLINABLE product #-}

elem :: (Monad m, Eq a) => a -> Fold a m Bool
elem = fromPure . Pure.elem
{-# INLINABLE elem #-}

notElem :: (Monad m, Eq a) => a -> Fold a m Bool
notElem = fromPure . Pure.notElem
{-# INLINABLE notElem #-}

genericLength :: (Monad m, Num b) => Fold a m b
genericLength = fromPure Pure.genericLength
{-# INLINABLE genericLength #-}
