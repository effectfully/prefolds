# prefolds

## Quick taste

With [`Control.Foldl`](https://hackage.haskell.org/package/foldl-1.2.1/docs/Control-Foldl.html) you can write

```
fold ((/) <$> sum <*> genericLength) [1..10^6]
```

and it'll stream.

With `prefolds` you can write

```
execute ((/) <$> take (10^6) sum <*> take (10^6) genericLength) [1..]
```

and it'll stream too. There are multiple ways to compose folds:

 1. `f <+> g` reads as "consume a stream by `f` and `g` in parallel, stop when both folds are saturated and apply the result of `f` to the result of `g`".
 2. `f <*> g` reads as "consume a stream by `f` and `g` in parallel, stop when either fold is saturated and apply the result of `f` to the result of `g`". That's what `Control.Foldl` has.
 3. `f </> g` reads as "consume a stream by `f`, then, when `f` is saturated, consume the rest of the stream by `g` and apply the result of `f` to the result of `g`".
 4. `scan f g` reads as "scan a stream with `f`, then consume the resulting stream by `g`".
 5. `groupBy p f g` reads as "groupBy elements of a stream by `p`, fold substreams by `f`, then fold the resulting stream by `g`".
 6. `inits f g` reads as "fold "inits" of a stream by `f`, then fold the resulting stream by `g`".
 7. `chunks f g` reads as "fold a stream with `f`, then, when `f` is saturated, fold the rest of the stream with `f`, then, when `f` is saturated... and then fold the resulting stream with `g`".

Here is an extended example:

```
-- Prints
-- 2
-- 4
-- 6
-- [120,12]
-- [7,8,9,10]
-- 11
example :: IO ()
example = executeM (final <$> sink1 <+> sink2 </> sink3 <*>> total) [1..] where
  final x y zs n = print [x,y] >> print zs >> print n
  sink1 = take 4 $ map succ product                     -- 2 * 3 * 4 * 5 = 120
  sink2 = take 6 . filter even $ traverse_ print *> sum -- 2 + 4 + 6 = 12
  sink3 = takeWhile (<= 10) list                        -- [7,8,9,10] (its length is 4)
  total = length -- total number of processed elements is 11, since
                 -- `takeWhile (<= 10)` forced `11` before it stopped.
```

Here we compose four streaming folds. `(<+>)` and others have the same associativity and fixity as `(<$>)`, so the fold is parsed as

```
((((final <$> sink1) <+> sink2) </> sink3) <*>> total)
```

This reads as follows:

 1. Consume a stream in parallel by `sink1` and `sink2` and stop when both are saturated.
 2. Consume the rest of the stream by `sink3`
 3. While performing 1. and 2. also consume the stream by `total` and stop when either 1. and 2. is stopped or `total` is saturated (which can't happen, since an attempt to find the length of an infinite list diverges).
 4. Collect results and pass them to `final`.

## Internals

`Fold` is defined almost as the one in `Control.Foldl`:

```
data Fold a m b = forall acc. Fold (acc -> m b) (acc -> a -> DriveT m acc) (DriveT m acc)
```

except that we have this `DriveT` transformer which turns a `Monad` into a "`MonoMonad`".

```
data Drive a = Stop !a | More !a
newtype DriveT m a = DriveT { getDriveT :: m (Drive a) }
```

If an accumulator is in the `Stop` state, then the fold is saturated. If accumulator is in the `More` state, then the fold can consume more input. `Drive` (and `DriveT m` for `Monad m`) is an `Applicative` in two ways:

```
instance Applicative Drive where
  pure = More
  
  More f <*> More x = More $ f x
  f      <*> x      = Stop $ runDrive f (runDrive x)

instance SumApplicative Drive where
  spure = Stop
  
  Stop f <+> Stop x = Stop $ f x
  f      <+> x      = More $ runDrive f (runDrive x)
```

`SumApplicative` has the same methods and laws as `Applicative` except methods are named differently. There are corresponding `Monad` and `SumMonad` instances, but they don't allow to terminate execution early (like with `Either`), because, well, how would you define `Stop x >>= f = Stop x` if `f :: a -> m b` when you're supposed to return `m b` and you have `Stop x :: m a`? So there is another type class:

class Functor m => MonoMonad m where
  mpure :: a -> m a
  default mpure :: Applicative m => a -> m a
  mpure = pure

  (>>#) :: m a -> (a -> m a) -> m a

With this we can define

```
instance MonoMonad Drive where  
  Stop x >># f = Stop x
  More x >># f = f x
```

`Drive` and `DriveT m` are also `Comonad`s:

```
instance Comonad Drive where
  extract = runDrive
  
  extend f = drive (Stop . f . Stop) (More . f . More)

instance Monad m => Comonad (DriveT m) where
  extract  = error "there is no `extract` for `DriveT m` unless `m` is a comonad, \
                   \ but this is not needed for `extend`, which is more important than `extract`"

  extend f = ...
```

The last instance is used a lot across the code.

There also some `MonadTrans`-like type classes: one for `MonoMonad` and the other for `SumMonad`:

```
class MonoMonadTrans t where
  mlift :: Monad m => m a -> t m a

class SumMonadTrans t where
  slift :: Monad m => m a -> t m a
```

Instances of these type classes

```
instance MonoMonadTrans DriveT where
  mlift a = DriveT $ More <$> a

instance SumMonadTrans  DriveT where
  slift a = DriveT $ Stop <$> a
```

are used in the code too.

Correspondingly, `Fold a m` is an `Applicative` and a `SumApplicative` (as you've seen in the example above) and `Fold a` is a `MonoMonadTrans` and a `SumMonadTrans`.

## Kleisli functors

As to that `(<*>>)`... have you ever wanted to apply `f :: a -> b -> m c` to `a :: m a` and `b :: m b` in applicative style and get `m c`? You can do `join $ f <$> a <*> b`, but this is kinda verbose. So we can define a special purpose combinator

```
(<*>>) :: m (a -> m b) -> m a -> m c
f <*>> a = f >>= (a >>=)
```

But there is a nice abstract structure behind this combinator, namely the one of [Kleisli Functors](https://elvishjerricco.github.io/2016/10/12/kleisli-functors.html):

```
class (Monad m, Functor f) => KleisliFunctor m f where
  kmap :: (a -> m b) -> f a -> f b
  kmap = kjoin .* fmap

  kjoin :: f (m a) -> f a
  kjoin = kmap id

(<$>>) :: KleisliFunctor m f => (a -> m b) -> f a -> f b
(<$>>) = kmap

(<*>>) :: (KleisliFunctor m f, Applicative f) => f (a -> m b) -> f a -> f b
h <*>> a = kjoin $ h <*> a

(<+>>) :: (KleisliFunctor m f, SumApplicative f) => f (a -> m b) -> f a -> f b
h <+>> a = kjoin $ h <+> a

instance Monad m => KleisliFunctor m m where
  kmap = (=<<)
```

And this instance

```
instance Monad m => KleisliFunctor m (Fold a m) where
  kmap h (Fold g f a) = Fold (g >=> h) f a
```

allows to use `(<*>>)` the way it's used above.