{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification, NoImplicitPrelude #-}
module Core where

import Lib
import Data.Strict.Tuple
import Data.Strict.Drive
import qualified Lib
import qualified Pure

infixl 4 </>, />, </>>

data Fold a m b = forall acc. Fold (acc -> m b) (acc -> a -> DriveT m acc) (DriveT m acc)

saturated_consumed :: String -> a
saturated_consumed name = error $ concat
  [ "prefolds."
  , name
  , ": a saturated fold consumed an element. "
  , "If you didn't define any functions that explicitly deal with `DriveT`, "
  , "then please report this as a bug. "
  , "If you did define such functions, "
  , "then it's likely that one of them doesn't stop on `Stop`."
  ]
{-# NOINLINE saturated_consumed #-}
-- I guess no reason to bloat code by inlining a function that throws an error?

driveFold :: Monad m => (b -> a -> DriveT m b) -> DriveT m b -> Fold a m b
driveFold = Fold pure
{-# INLINEABLE driveFold #-}

driveHalt :: Monad m => DriveT m b -> Fold a m b
driveHalt = driveFold $ saturated_consumed "driveHalt"
{-# INLINABLE driveHalt #-}

driveMore :: Monad m => DriveT m b -> Fold a m b
driveMore = driveFold $ more .* const
{-# INLINABLE driveMore #-}

instance Monad m => Functor (Fold a m) where
  -- `h <.> g` is not strictified, because I'm not sure it's needed.
  -- The same applies to other instances.
  fmap h (Fold g f a) = Fold (h <.> g) f a
  {-# INLINEABLE fmap #-}

instance Monad m => KleisliFunctor m (Fold a m) where
  kmap h (Fold g f a) = Fold (g >=> h) f a
  {-# INLINEABLE kmap #-}

combine :: Monad m
        => (forall a. (a -> a) -> a -> a)
        -> Fold a m (b -> c) -> Fold a m b -> Fold a m c
combine c (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step acc where
  acc = isStopT a1 >>~ \b -> Triple b <$> a1 <+> a2

  step (Triple b a1' a2') x
    | b         = Triple True a1' <$> f2 a2' x
    | otherwise = driveDriveT (\a1'' -> Triple True a1'' <$> c (>># flip f2 x) a2)
                              (\a1'' -> more $ Triple False a1'' a2')
                              (f1 a1' x)

  final (Triple b a1' a2') = g1 a1' <*> g2 a2'
{-# INLINABLE combine #-}

(</>) :: Monad m => Fold a m (b -> c) -> Fold a m b -> Fold a m c
(</>) = combine id
{-# INLINEABLE (</>) #-}

(/>) :: Monad m => Fold a m b -> Fold a m c -> Fold a m c
b /> c = const id <$> b </> c
{-# INLINEABLE (/>) #-}

(</>>) :: Monad m => Fold a m (b -> m c) -> Fold a m b -> Fold a m c
(</>>) = kjoin .* (</>)
{-# INLINEABLE (</>>) #-}

instance Monad m => Applicative (Fold a m) where
  pure = driveHalt . spure
  {-# INLINABLE pure #-}

  (<*>) = combine (const id)
  {-# INLINABLE (<*>) #-}
  
instance Monad m => SumApplicative (Fold a m) where
  spure = driveHalt . spure
  {-# INLINABLE spure #-}

  Fold g1 f1 a1 <+> Fold g2 f2 a2 = Fold final step (pairW a1 a2) where
    pairW a1 a2 = Pair <$> duplicate a1 <+> duplicate a2

    step (Pair a1' a2') x = pairW (a1' >># flip f1 x) (a2' >># flip f2 x)

    final (Pair a1' a2') = (a1' >>~ g1) <*> (a2' >>~ g2)
  {-# INLINABLE (<+>) #-}

instance Monad m => AndApplicative (Fold a m) where
  apure = driveMore . apure
  {-# INLINABLE apure #-}

  Fold g1 f1 a1 <&> Fold g2 f2 a2 = Fold final step (Pair <$> a1 <&> a2) where
    step (Pair a1' a2') x = Pair <$> f1 a1' x <&> f2 a2' x

    final (Pair a1' a2') = g1 a1' <*> g2 a2'
  {-# INLINABLE (<&>) #-}

-- There isn't much point in making `acc` strict I guess, but it costs nothing, so why not.
data FoldMore a m b = forall acc. FoldMore (acc -> m b) (acc -> a -> DriveT m acc) !acc

runFoldMore :: FoldMore a m b -> m b
runFoldMore (FoldMore g f a) = g a
{-# INLINEABLE runFoldMore #-}

toFoldMore :: Functor m => Fold a m b -> DriveT m (FoldMore a m b)
toFoldMore (Fold g f (DriveT a)) = DriveT $ fmap (FoldMore g f) <$> a
{-# INLINEABLE toFoldMore #-}

feedFoldMore :: Functor m => a -> FoldMore a m b -> DriveT m (FoldMore a m b)
feedFoldMore x (FoldMore g f a) = FoldMore g f <$> f a x
{-# INLINEABLE feedFoldMore #-}

instance Monad m => Monad (Fold a m) where
  Fold g1 f1 a1 >>= h = Fold final step (left a1) where
    left = driveDriveT (g1 >~> Right <.> toFoldMore . h) (more . Left)

    step (Left a1') x = left $ f1 a1' x
    step (Right f2) x = Right <$> feedFoldMore x f2

    final (Left a1') = g1 a1' >>= runFold . h
    final (Right f2) = runFoldMore f2
  {-# INLINABLE (>>=) #-}

instance SumMonadTrans (Fold a) where
  slift = driveMore . slift
  {-# INLINABLE slift #-}

instance AndMonadTrans (Fold a) where
  alift = driveHalt . alift
  {-# INLINABLE alift #-}

instance MFunctor (Fold a) where
  hoist h (Fold g f a) = Fold (h . g) (hoist h .* f) (hoist h a)
  {-# INLINEABLE hoist #-}

runFold :: Monad m => Fold a m b -> m b
runFold (Fold g f a) = a >>~ g
{-# INLINEABLE runFold #-}

feedFold :: Monad m => a -> Fold a m b -> DriveT m (Fold a m b)
feedFold x (Fold g f a) = a >># flip f x =>> Fold g f
{-# INLINEABLE feedFold #-}

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
spanM h p b c = h <$> takeWhile p b </>> c
{-# INLINEABLE spanM #-}

span :: Monad m => (b -> c -> d) -> (a -> Bool) -> Fold a m b -> Fold a m c -> Fold a m d
span f = spanM (pure .* f)
{-# INLINEABLE span #-}

span_ :: Monad m => (a -> Bool) -> Fold a m b -> Fold a m c -> Fold a m c
span_ = span (const id)
{-# INLINEABLE span_ #-}

take :: Monad m => Int -> Fold a m b -> Fold a m b
take n (Fold g f a) = Fold (g . sndp) step (finish n a) where
  finish n a = terminateWhen ((<= 0) . fstp) $ Pair n <$> a

  step (Pair n a) x | n <= 0    = saturated_consumed "take"
                    | otherwise = finish (n - 1) $ f a x
{-# INLINABLE take #-}
  
drop :: Monad m => Int -> Fold a m b -> Fold a m b
drop n (Fold g f a) = Fold (g . sndp) step (Pair n <$> a) where
  step (Pair n a) x | n <= 0    = Pair n <$> f a x
                    | otherwise = more $ Pair (n - 1) a
{-# INLINABLE drop #-}

cross :: Monad m => m a -> DriveT m b -> (b -> a -> DriveT m b) -> DriveT m b
cross a b f = a >>~ \x -> b >># flip f x
{-# INLINABLE cross #-}

scan :: Monad m => Fold a m b -> Fold b m c -> Fold a m c
scan (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step (pair a1 a2) where
  pair a1 a2 = Pair <$> a1 <&> duplicate a2

  step (Pair a1' a2') x = pair (f1 a1' x) $ cross (g1 a1') a2' f2

  final (Pair a1' a2') = cross (g1 a1') a2' f2 >>~ g2
{-# INLINEABLE scan #-}

-- Unlike the prelude version, `p` must be only transitive and is not required to be symmetric.
groupBy :: Monad m => (a -> a -> Bool) -> Fold a m b -> Fold b m c -> Fold a m c
groupBy p (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step acc where
  acc = a2 =>> Quadruple False (const True) a1

  step (Quadruple _ p' a1' a2') x
    | p' x      = more $ pair a1' a2'
    | otherwise = cross (a1' >>~ g1) a2' f2 =>> pair a1
    where pair a = Quadruple True (p x) (a >># flip f1 x)

  final (Quadruple b _ a1' a2') = (if b then cross (a1' >>~ g1) a2' f2 else a2') >>~ g2
{-# INLINABLE groupBy #-}

-- Same as `groupBy`, but is slightly more efficient.
-- The only difference is that this version emulates `Prelude.groupBy p [] = [[]]`.
groupBy1 :: Monad m => (a -> a -> Bool) -> Fold a m b -> Fold b m c -> Fold a m c
groupBy1 p (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step acc where
  acc = a2 =>> Triple (const True) a1

  step (Triple p' a1' a2') x
    | p' x      = more $ pair a1' a2'
    | otherwise = cross (a1' >>~ g1) a2' f2 =>> pair a1
    where pair a = Triple (p x) (a >># flip f1 x)

  final (Triple _ a1' a2') = cross (a1' >>~ g1) a2' f2 >>~ g2
{-# INLINABLE groupBy1 #-}

group :: (Monad m, Eq a) => Fold a m b -> Fold b m c -> Fold a m c
group = groupBy (==)
{-# INLINEABLE group #-}

inits :: Monad m => Fold a m b -> Fold b m c -> Fold a m c
inits (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step (a2 =>> Pair a1) where
  step (Pair a1' a2') x = cross (a1' >>~ g1) a2' f2 =>> Pair (a1' >># flip f1 x)

  final (Pair a1' a2') = cross (a1' >>~ g1) a2' f2 >>~ g2
{-# INLINABLE inits #-}

chunks :: Monad m => Fold a m b -> Fold b m c -> Fold a m c
chunks (Fold g1 f1 a1) (Fold g2 f2 a2) = Fold final step (init a2) where
  init a2' = Triple False <$> a1 <&> duplicate a2'

  step (Triple _ a1' a2') x = driveDriveT (\a1'' -> init $ cross (g1 a1'') a2' f2)
                                          (\a1'' -> more $ Triple True a1'' a2')
                                          (f1 a1' x)

  final (Triple b a1' a2') = (if b then cross (g1 a1') a2' f2 else a2') >>~ g2
{-# INLINABLE chunks #-}

chunksOf :: Monad m => Int -> Fold a m b -> Fold b m c -> Fold a m c
chunksOf = chunks .* take
{-# INLINABLE chunksOf #-}

splitWhen :: Monad m => (a -> Bool) -> Fold a m b -> Fold b m c -> Fold a m c
splitWhen = chunks .* takeWhile
{-# INLINABLE splitWhen #-}

splitOne :: (Monad m, Eq a) => a -> Fold a m b -> Fold b m c -> Fold a m c
splitOne = splitWhen . (/=)
{-# INLINABLE splitOne #-}

execM :: (Monad m, Foldable t) => Fold a m b -> t a -> m b
execM (Fold g f a) xs = a >># flip (mfoldM f) xs >>~ g
{-# INLINABLE execM #-}

exec :: Foldable t => Fold a Identity b -> t a -> b
exec = runIdentity .* execM
{-# INLINABLE exec #-}

impurely :: Monad m
         => (forall acc. (DriveT m acc -> (acc -> m b) -> m b) ->
               (acc -> m b) -> (acc -> a -> DriveT m acc) -> DriveT m acc -> c)
         -> Fold a m b -> c
impurely h (Fold g f a) = h (flip $ driveTM g) g f a
{-# INLINABLE impurely #-}

impurelyRest :: Monad m
             => (forall acc. (forall b. (acc -> m b) -> (acc -> m b) -> DriveT m acc -> m b) ->
                   (acc -> m b) -> (acc -> a -> DriveT m acc) -> DriveT m acc -> c)
             -> Fold a m b -> c
impurelyRest h (Fold g f a) = h driveTM g f a
{-# INLINABLE impurelyRest #-}

newtype Pattern m a z = Pattern { getPattern :: a -> DriveT m a }

instance Functor (Pattern m acc) where
  fmap _ (Pattern f) = Pattern f
  {-# INLINE fmap #-}

instance Monad m => Applicative (Pattern m acc) where
  pure _ = Pattern more
  {-# INLINE pure #-}

  Pattern f <*> Pattern g = Pattern (f >#> g)
  {-# INLINE (<*>) #-}

type Handler b m a = forall acc. (a -> Pattern m acc a) -> b -> Pattern m acc b

handle :: Monad m => Handler b m a -> Fold a m c -> Fold b m c
handle k (Fold g f a) = Fold g (flip $ getPattern . k (Pattern . flip f)) a
{-# INLINEABLE handle #-}
