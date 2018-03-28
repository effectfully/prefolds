# prefolds

## A quick taste

With [`Control.Foldl`](https://hackage.haskell.org/package/foldl-1.2.1/docs/Control-Foldl.html) you can write

```haskell
fold ((/) <$> sum <*> genericLength) [1..10^6]
```

and it'll stream.

With `prefolds` you can write

```haskell
exec ((/) <$> take (10^6) sum <&> take (10^6) genericLength) [1..]
```

and it'll stream too.

With `Control.Foldl` `fold null [1..10^6]` will traverse the entire list. With `prefolds` the following holds: `exec null (1:undefined) ≡ False`. As well as `exec (take 0 undefined) ≡ []`. And folds are monadic, e.g. `exec (take 2 sum >>= \n -> take n list) [1..6] ≡ [3..5]`.

## Overview

There are multiple ways to compose folds:

 1. `f <+> g` reads as "consume a stream by `f` and `g` in parallel, stop when both folds are saturated and apply the result of `f` to the result of `g`".
 2. `f <&> g` reads as "consume a stream by `f` and `g` in parallel, stop when either fold is saturated and apply the result of `f` to the result of `g`". That's what `Control.Foldl` has.
 3. `f <*> g` reads as "consume a stream by `f`, then, when `f` is saturated, consume the rest of the stream by `g` and apply the result of `f` to the result of `g`".
 4. `f >>= h` reads as "consume a stream by `f`, then, when `f` is saturated, pass the result to `h`
and consume the rest of the stream by the resulting fold.
 4. `scan f g` reads as "scan a stream with `f`, then consume the resulting stream by `g`".
 5. `groupBy p f g` reads as "groupBy elements of a stream by `p`, fold substreams by `f`, then fold the resulting stream by `g`".
 6. `inits f g` reads as "fold "inits" of a stream by `f`, then fold the resulting stream by `g`".
 7. `chunks f g` reads as "fold a stream with `f`, then, when `f` is saturated, fold the rest of the stream with `f`, then, when `f` is saturated... and then fold the resulting stream with `g`".

Here is an extended example:

```haskell
-- Prints
-- 2
-- 4
-- 6
-- [120,12]
-- [7,8,9,10]
-- 11
example :: IO ()
example = execM (final <$> sink1 <+> sink2 <*> sink3 <&>> total) [1..] where
  final x y zs n = print [x,y] >> print zs >> print n
  sink1 = take 4 $ map succ product                     -- 2 * 3 * 4 * 5 = 120
  sink2 = take 6 . filter even $ traverse_ print &> sum -- 2 + 4 + 6 = 12
  sink3 = takeWhile (<= 10) list                        -- [7,8,9,10]
  total = length -- total number of processed elements is 11, since
                 -- `takeWhile (<= 10)` forced `11` before it stopped.
```

Here we compose four streaming folds. `(<+>)` and others have the same associativity and fixity as `(<$>)`, so the fold is parsed as

```haskell
((((final <$> sink1) <+> sink2) <*> sink3) <&>> total)
```

This reads as follows:

 1. Consume a stream in parallel by `sink1` and `sink2` and stop when both are saturated.
 2. Consume the rest of the stream by `sink3`
 3. While performing 1. and 2. also consume the stream by `total` and stop when either 1. and 2. is stopped or `total` is saturated (which can't happen, since an attempt to find the length of an infinite list diverges).
 4. Collect results and pass them to `final`.

## Internals

`Fold` is defined almost as the one in `Control.Foldl`:

```haskell
data Fold a m b = forall acc. Fold (acc -> m b) (acc -> a -> DriveT m acc) (DriveT m acc)
```

except that we have this `DriveT` transformer which turns a `Monad` into a "`MonoMonad`".

```haskell
data Drive a = Stop !a | More !a
newtype DriveT m a = DriveT { getDriveT :: m (Drive a) }
```

If an accumulator is in the `Stop` state, then the fold is saturated. If an accumulator is in the `More` state, then the fold can consume more input. `Drive` (and `DriveT m` for `Monad m`) is an `Applicative` in two ways:

```haskell
instance SumApplicative Drive where
  spure = Stop

  Stop f <+> Stop x = Stop $ f x
  f      <+> x      = More $ runDrive f (runDrive x)

instance AndApplicative Drive where
  apure = More

  More f <&> More x = More $ f x
  f      <&> x      = Stop $ runDrive f (runDrive x)
```

`SumApplicative` and `AndApplicative` have the same methods and laws as `Applicative` except methods are named differently. There are corresponding `SumMonad` and `AndMonad` instances, but they don't allow to terminate execution early (like with `Either`), because, well, how would you define `Stop x >>= f = Stop x` if `f :: a -> m b` and you're supposed to return a `m b`, but `Stop x :: m a`? So there is another type class:

```haskell
-- The usual monad laws.
class Functor m => MonoMonad m where
  mpure :: a -> m a
  (>>#) :: m a -> (a -> m a) -> m a

  (>#>) :: (a -> m a) -> (a -> m a) -> a -> m a
  f >#> g = \x -> f x >># g
```

With this we can define

```haskell
instance MonoMonad Drive where
  Stop x >># f = Stop x
  More x >># f = f x
```

`Drive` and `DriveT m` are also `Comonad`s:

```haskell
instance Comonad Drive where
  extract = runDrive
  
  extend f = drive (Stop . f . Stop) (More . f . More)

instance Monad m => Comonad (DriveT m) where
  extract  = error "there is no `extract` for `DriveT m` unless `m` is a comonad, \
                   \ but this is not needed for `extend`, which is more important than `extract`"

  extend f = ...
```

The last instance is used a lot across the code.

There are also some `MonadTrans`-like type classes:

```haskell
-- Transforms a Monad into a  SumApplicative.
class SumApplicativeTrans t where
  slift :: Monad m => m a -> t m a

-- Transforms a Monad into an AndApplicative.
class AndApplicativeTrans t where
  mlift :: Monad m => m a -> t m a
```

Instances of these type classes:

```haskell
instance SumApplicativeTrans  DriveT where
  slift a = DriveT $ Stop <$> a

instance AndApplicativeTrans DriveT where
  mlift a = DriveT $ More <$> a
```

Here are some suggestive synonyms:

```haskell
halt :: SumApplicative f => a -> f a
halt = spure

more :: MonoMonad m => a -> m a
more = mpure

haltWhen :: (SumApplicative m, MonoMonad m) => (a -> Bool) -> a -> m a
haltWhen p x = if p x then halt x else more x

moreWhen :: (SumApplicative m, MonoMonad m) => (a -> Bool) -> a -> m a
moreWhen p x = if p x then more x else halt x

stop :: (SumApplicativeTrans t, Monad m) => m a -> t m a
stop = slift

keep :: (AndApplicativeTrans t, Monad m) => m a -> t m a
keep = alift

terminate :: (SumApplicative m, MonoMonad m) => m a -> m a
terminate a = a >># halt

terminateWhen :: (SumApplicative m, MonoMonad m) => (a -> Bool) -> m a -> m a
terminateWhen p a = a >># \x -> if p x then halt x else more x
```

`Fold a m` is a `Monad`, a `SumApplicative` and an `AndApplicative` (as you've seen in the example above) and `Fold a` is a `SumApplicativeTrans` and an `AndApplicativeTrans`.

## Kleisli functors

As to that `(<&>>)`... have you ever wanted to apply `f :: a -> b -> m c` to `a :: m a` and `b :: m b` in applicative style and get `m c`? You can do `join $ f <$> a <*> b`, but this is kinda verbose. So we can define a special purpose combinator

```haskell
(<*>>) :: Monad m => m (a -> m b) -> m a -> m c
f <*>> a = f >>= (a >>=)
```

But there is a nice abstract structure behind this combinator that gives us such combinators for all our Applicative-like classes, namely the one of [Kleisli Functors](https://elvishjerricco.github.io/2016/10/12/kleisli-functors.html):

```haskell
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

(<&>>) :: (KleisliFunctor m f, AndApplicative f) => f (a -> m b) -> f a -> f b
h <&>> a = kjoin $ h <&> a

instance Monad m => KleisliFunctor m m where
  kmap = (=<<)
```

And this instance

```haskell
instance Monad m => KleisliFunctor m (Fold a m) where
  kmap h (Fold g f a) = Fold (g >=> h) f a
```

allows to use `(<&>>)` the way it's used above.
