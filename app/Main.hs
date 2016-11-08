{-# LANGUAGE NoImplicitPrelude #-}
module Main where

-- I'll (hopefully) write some proper tests.

import Lib as P
import Fold
import Data.Monoid
import Data.Functor.Identity
import qualified Prelude as P
import qualified Data.List as P

test2 :: IO ()
test2 = executeM (take 3 $ traverse_ print) [1..]

test3 :: IO ()
test3 = executeM (groupBy (<) (foldMap Sum) $ traverse_ print) $ [1,2,3,2,4,6,5,8,3,1,2,3]

test4 :: IO ()
test4 = executeM (take 15 . groupBy (<) (take 2 $ foldMap Sum) $ traverse_ print) $ [1,2,3,2,4,6,5,8,3] ++ [1..]

test5 :: IO ()
test5 = executeM (takeWhile (<= 10) . dropWhile (<= 3) . filter even $ traverse_ print) $ [1..]

test6 :: IO ()
test6 = executeM (take 3 . groupBy (<) (foldMap Sum) $ traverse_ print) $ [1,2,3,4]

test7 :: IO ()
test7 = executeM (groupBy (<) (foldMap Sum) . take 4 $ traverse_ print) $ [1,2,3,2,4,6,5,8,3] ++ [1..]

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

main = nofail1
