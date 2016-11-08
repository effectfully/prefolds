{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification, TypeOperators #-}
module Main where

import Lib as P
import Fold
import Data.Monoid
import Data.Functor.Identity
import qualified Prelude as P
import qualified Data.List as P
import Control.Monad.Trans.Writer

data Test = forall a. (Show a, Eq a) => a :== a
          | Label String [Test]

type Suite = Writer [Test] ()

runSuite :: Suite -> String
runSuite = fin . gos . execWriter where
  fin [] = "OK"
  fin ss = P.intercalate "\n" ss

  gos = concatMap go . zip [1..]

  go (i, x :== y)    = [concat [show i, ": ", show x, " is not ", show y] | x /= y]
  go (i, Label s ts) = P.map (\t -> s ++ "/" ++ t) $ gos ts

label :: String -> Suite -> Suite
label s t = tell [Label s $ execWriter t]

(===) :: (Show a, Eq a) => a -> a -> Suite
x === y = tell [x :== y]

main = test8


perform :: (Fold a Identity [a] -> Fold a Identity b) -> [a] -> b
perform f = execute (f list)

behaviour :: IO ()
behaviour = putStrLn . runSuite $ do
  label "map"  $ do
    perform (map (^2)) []     === ([] :: [Integer])
    perform (map (^2)) [1..3] === [1,4,9]
  label "take" $ do
    perform (take 0) []     === ([] :: [Integer])
    perform (take 1) []     === ([] :: [Integer])
    perform (take 0) [1..]  === []
    perform (take 3) [1,2]  === [1,2]
    perform (take 3) [1..3] === [1..3]
    perform (take 3) [1..]  === [1..3]
  label "filter" $ do
    perform (filter even) []     === []
    perform (filter even) [1..5] === [2,4]
  label "map-take-filter" $ do
    perform (map (* 3) . take 5 . filter even) [1..] === [6,12]
    perform (map (* 3) . filter even . take 5) [1..] === [6,12,18,24,30]
    perform (take 5 . map (* 3) . filter even) [1..] === [6,12]
    perform (take 5 . filter even . map (* 3)) [1..] === [6,12]
    perform (filter even . map (* 3) . take 5) [1..] === [6,12,18,24,30]
    perform (filter even . take 5 . map (* 3)) [1..] === [6,12,18,24,30]
  label "takeWhile" $ do
    perform (takeWhile (<= 5)) []     === []
    perform (takeWhile (<= 5)) [1..4] === [1..4]
    perform (takeWhile (<= 5)) [1..6] === [1..5]
    perform (takeWhile (<= 5)) [5..]  === [5]
    perform (takeWhile (<= 5)) [6..]  === []
  label "groupBy" $ do
    label "basic" $ do
      perform (groupBy (<) sum) []                            === []
      execute (groupBy (<) list list) []                      === ([] :: [[Integer]])
      execute (groupBy (<) list list) [1]                     === [[1]]
      execute (groupBy (<) list list) [1,2]                   === [[1,2]]
      execute (groupBy (<) list list) [1,3,2]                 === [[1,3],[2]]
      execute (groupBy (<) list list) [2,1,3]                 === [[2],[1,3]]
      execute (groupBy (<) list list) [2,1,3,4,5]             === [[2],[1,3,4,5]]
      execute (groupBy (<) list list) [2,1,3,4,5,4,2,1,4,6,8] === [[2],[1,3,4,5],[4],[2],[1,4,6,8]]
      execute (groupBy (<) list list) [1..5]                  === [[1..5]]
    label "stop" $ do
      let xs = [7,1,2,3,4,3,5,9,2] ++ [1..]
      execute (groupBy (<) (take 0 list) $ take 0 list)  xs === []
      execute (groupBy (<) (take 0 list) $ take 1 list)  xs === [[]]
      execute (groupBy (<) (take 0 list) $ take 3 list)  xs === [[],[],[]]
      execute (groupBy (<) (take 1 list) $ take 0 list)  xs === []
      execute (groupBy (<) (take 3 list) $ take 0 list)  xs === []
      execute (groupBy (<) (take 1 list) $ take 1 list)  xs === [[7]]
      execute (groupBy (<) (take 2 list) $ take 3 list)  xs === [[7],[1,2],[3,5]]
      execute (groupBy (<) (take 3 list) $ take 2 list)  xs === [[7],[1,2,3]]
      execute (groupBy (<) (take 3 list) $ take 3 list)  xs === [[7],[1,2,3],[3,5,9]]
      execute (take 12 $ groupBy (<)  list         list) xs === [[7],[1,2,3,4],[3,5,9],[2],[1,2,3]]
      execute (take 12 $ groupBy (<) (take 2 list) list) xs === [[7],[1,2],[3,5],[2],[1,2]]
  label "groupBy1" $ do
    label "basic" $ do
      perform (groupBy1 (<) sum) []                            === [0]
      execute (groupBy1 (<) list list) []                      === [[] :: [Integer]]
      execute (groupBy1 (<) list list) [1]                     === [[1]]
      execute (groupBy1 (<) list list) [1,2]                   === [[1,2]]
      execute (groupBy1 (<) list list) [1,3,2]                 === [[1,3],[2]]
      execute (groupBy1 (<) list list) [2,1,3]                 === [[2],[1,3]]
      execute (groupBy1 (<) list list) [2,1,3,4,5]             === [[2],[1,3,4,5]]
      execute (groupBy1 (<) list list) [2,1,3,4,5,4,2,1,4,6,8] === [[2],[1,3,4,5],[4],[2],[1,4,6,8]]
      execute (groupBy1 (<) list list) [1..5]                  === [[1..5]]
    label "stop" $ do
      let xs = [7,1,2,3,4,3,5,9,2] ++ [1..]
      execute (groupBy1 (<) (take 0 list) $ take 0 list)  xs === []
      execute (groupBy1 (<) (take 0 list) $ take 1 list)  xs === [[]]
      execute (groupBy1 (<) (take 0 list) $ take 3 list)  xs === [[],[],[]]
      execute (groupBy1 (<) (take 1 list) $ take 0 list)  xs === []
      execute (groupBy1 (<) (take 3 list) $ take 0 list)  xs === []
      execute (groupBy1 (<) (take 1 list) $ take 1 list)  xs === [[7]]
      execute (groupBy1 (<) (take 2 list) $ take 3 list)  xs === [[7],[1,2],[3,5]]
      execute (groupBy1 (<) (take 3 list) $ take 2 list)  xs === [[7],[1,2,3]]
      execute (groupBy1 (<) (take 3 list) $ take 3 list)  xs === [[7],[1,2,3],[3,5,9]]
      execute (take 12 $ groupBy1 (<)  list         list) xs === [[7],[1,2,3,4],[3,5,9],[2],[1,2,3]]
      execute (take 12 $ groupBy1 (<) (take 2 list) list) xs === [[7],[1,2],[3,5],[2],[1,2]]

test5 :: IO ()
test5 = executeM (takeWhile (<= 10) . dropWhile (<= 3) . filter even $ traverse_ print) $ [1..]

-- 1.2s elapsed.
test8 :: IO ()
test8 = print . getSum $
          execute (take (10^7) . group (foldMap Sum) $ sum) $
            cycle $ replicate 10 1 ++ replicate 10 (2 :: Integer)

-- 1.35s elapsed.
test9 :: IO ()
test9 = print $
          P.sum . P.map (getSum . P.foldMap Sum) . P.group . P.take (10^7) $
            cycle $ replicate 10 1 ++ replicate 10 (2 :: Integer)

{-
test10 :: IO ()
test10 = print . runIdentity $ executeM ((/) <$> sum <*> genericLength) [1..10^7]

-- 3 MB total memory in use.
test11 :: IO ()
test11 = mapM_ print
  [ execute ((/) <$> take (10^7) sum <*> take (10^6) genericLength) [1..]
  , P.sum [1..10^7] / fromIntegral (P.length [1..10^6])
  ]

-- The first three perform roughly the same, the last two are more than two times faster
-- (which is OK, I guess).
test14 :: IO ()
test14 = mapM_ print
  [ execute (scan (take (10^6-1) sum) $ sum) [1..]
  , execute (scan sum . take (10^6)   $ sum) [1..]
  , execute (take (10^6-1) . scan sum $ sum) [1..]
  , P.sum . P.scanl' (+) 0 $ P.take (10^6-1) [1..]
  , P.sum . P.take (10^6) $ P.scanl' (+) 0   [1..]
  ]

test15 :: IO ()
test15 = mapM_ print
  [ execute (filter even $ scan sum list) [1..5]
  , execute (scan sum $ filter even list) [1..5]
  ]

test16 :: IO ()
test16 = executeM (   (takeWhile (<= 5) . map (^2) $ traverse_ print)
                   /> (take 3 $ traverse_ print)) [1..]

test17 :: IO ()
test17 = executeM (    (takeWhile (<= 5) . map (^2) $ traverse_ print)
                   //> (take 0 $ traverse_ print)) [1..]

test18 :: IO ()
test18 = executeM (take 0 sum //> take 1 (traverse_ print)) [1..]

-- 140 MB total memory in use.
fail1 :: IO ()
fail1 = print . uncurry (\xs ys -> P.sum xs + P.sum ys) . P.span (1 ==) $
          replicate (10^7) (1 :: Integer)

-- 2 MB total memory in use.
nofail1 :: IO ()
nofail1 = print . execute (span (+) (1 ==) sum sum) $ replicate (10^7) (1 :: Integer)

test19 :: IO ()
test19 = executeM (span_ (< 4) (map (^2) $ traverse_ print) (take 5 $ traverse_ print)) [1..]

-- Prints
-- 2
-- 4
-- 6
-- [120,12]
-- [7,8,9,10]
test20 :: IO ()
test20 = executeM (final <$> sink1 <*> sink2 </>> sink3) [1..] where
  final x y zs = print [x,y] >> print zs 
  sink1 = take 4 $ map succ product                     -- 2 * 3 * 4 * 5 = 120
  sink2 = take 6 . filter even $ traverse_ print *> sum -- 2 + 4 + 6 = 12
  sink3 = takeWhile (<= 10) list                        -- [7, 8, 9, 10]

main = nofail1
-}
