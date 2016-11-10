{-# LANGUAGE NoImplicitPrelude #-}
module Bench where

import Lib as P
import Fold
import Data.Monoid
import qualified Prelude as P
import qualified Data.List as P
import Criterion.Main

whnfFrom1To :: ([Integer] -> b) -> Integer -> Benchmarkable
whnfFrom1To f = whnf (f . enumFromTo 1)
{-# INLINE whnfFrom1To #-}

whnfFrom1 :: (Int -> [Integer] -> b) -> Int -> Benchmarkable
whnfFrom1 f = whnf (\n -> f n [n `seq` 1..])
{-# INLINE whnfFrom1 #-}

-- Prelude version is 20% faster.
benchSum :: Benchmark
benchSum = bgroup "sum"
  [ bench "Prefolds" $ whnfFrom1To (execute sum) (10^7)
  , bench "Prelude"  $ whnfFrom1To  P.sum        (10^7)
  ]

-- Prelude version is 20% slower.
benchAverage :: Benchmark
benchAverage = bgroup "average"
  [ bench "Prefolds" $ whnf  average (10^7)
  , bench "Prelude"  $ whnf paverage (10^7)
  ] where
      average  n = execute ((/) <$> sum <*> genericLength) [1..n]
      paverage n = P.sum [1..n] / fromIntegral (P.length [1..n])

-- Prelude version is more than two times faster.
benchAverageTake :: Benchmark
benchAverageTake = bgroup "averageTake"
  [ bench "Prefolds" $ whnf  average (10^7)
  , bench "Prelude"  $ whnf paverage (10^7)
  ] where
      average  n = execute ((/) <$> take n sum <*> take n genericLength) [n `seq` 1..]
      paverage n = P.sum (P.take n [n `seq` 1..])
                 / fromIntegral (P.length $ P.take n [n `seq` 1..])

-- All are equal (I expected `Prefolds/Sum` to be slower).
benchSlowAverageTake :: Benchmark
benchSlowAverageTake = bgroup "slowAverageTake"
  [ bench "Prefolds/Mul" $ whnf average  (10^4)
  , bench "Prefolds/Sum" $ whnf average' (10^4)
  , bench "Prelude"      $ whnf paverage (10^4)
  ] where
      average  n = execute ((/) <$> map slowId (take n sum) <*> take n genericLength) [n `seq` 1..]
      average' n = execute ((/) <$> map slowId (take n sum) <+> take n genericLength) [n `seq` 1..]
      paverage n = (P.sum . P.take n $ P.map slowId [n `seq` 1..])
                 / fromIntegral (P.length $ P.take n [n `seq` 1..])
      
      slowId :: (Eq a, Num a) => a -> a
      slowId n = go 1000 n where
        go 0 n = n
        go m n = go (m - 1) n

-- Prelude version is 10% slower.
benchGroup :: Benchmark
benchGroup = bgroup "group"
  [ bench "Prefolds.group" . flip whnf (gen 10) $
      getSum . execute (take (10^7) . group (foldMap Sum) $ sum)
  , bench "Prelude.group"  . flip whnf (gen 10) $
      P.sum . P.map (getSum . P.foldMap Sum) . P.group . P.take (10^7)
  ] where
      gen n = cycle $ replicate n 1 ++ replicate n 2

-- Prefolds versions are nearly equal, Prelude versions are two times faster.
benchScan :: Benchmark
benchScan = bgroup "scan"
  [ bench "Prefolds.scan/1" $ whnfFrom1 (\n -> execute $ scan (take n sum) sum)   (10^6-1)
  , bench "Prefolds.scan/2" $ whnfFrom1 (\n -> execute $ scan sum (take n sum))   (10^6-1)
  , bench "Prefolds.scan/3" $ whnfFrom1 (\n -> execute $ take n (scan sum sum))   (10^6-1)
  , bench "Prelude.scan/1"  $ whnfFrom1 (\n -> P.sum . P.scanl' (+) 0 . P.take n) (10^6-1)
  , bench "Prelude.scan/1"  $ whnfFrom1 (\n -> P.sum . P.take n . P.scanl' (+) 0) (10^6-1)
  ]

suite :: [Benchmark]
suite =
  [ benchSum
  , benchAverage
  , benchAverageTake
  , benchSlowAverageTake
  , benchGroup
  , benchScan
  ]

benchSuite :: IO ()
benchSuite = defaultMain suite
