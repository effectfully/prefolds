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
test2 = prefoldM (take 3 $ traverse_ print) [1..]

test3 :: IO ()
test3 = prefoldM (groupBy (<) (take 2 $ foldMap Sum) $ traverse_ print) $ [1,2,3,2,4,6,5,8,3,1,2,3]

test4 :: IO ()
test4 = prefoldM (take 15 . groupBy (<) (take 2 $ foldMap Sum) $ traverse_ print) $ [1,2,3,2,4,6,5,8,3] ++ [1..]

test5 :: IO ()
test5 = prefoldM (takeWhile (<= 10) . dropWhile (<= 3) . filter even $ traverse_ print) $ [1..]

test6 :: IO ()
test6 = prefoldM (take 3 . groupBy (<) (foldMap Sum) $ traverse_ print) $ [1,2,3,4]

test7 :: IO ()
test7 = prefoldM (groupBy (<) (foldMap Sum) . take 4 $ traverse_ print) $ [1,2,3,2,4,6,5,8,3] ++ [1..]

-- 1.2s elapsed.
test8 :: IO ()
test8 = print . getSum $
          prefold (take (10^7) . group (foldMap Sum) $ sum) $
            cycle $ replicate 10 1 ++ replicate 10 (2 :: Integer)

-- 1.35s elapsed.
test9 :: IO ()
test9 = print $
          P.sum . P.map (getSum . P.foldMap Sum) . P.group . P.take (10^7) $
            cycle $ replicate 10 1 ++ replicate 10 (2 :: Integer)

-- 140 MB total memory in use.
fail1 :: IO ()
fail1 = print $ uncurry (\xs ys -> P.sum xs + P.sum ys) . span (1 ==) $ replicate (10^7) (1 :: Integer)

test10 :: IO ()
test10 = print . runIdentity $ prefoldM ((/) <$> sum <*> genericLength) [1..10^7]

-- 3 MB total memory in use.
test11 :: IO ()
test11 = mapM_ print
  [ prefold ((/) <$> take (10^7) sum <*> take (10^6) genericLength) [1..]
  , P.sum [1..10^7] / fromIntegral (P.length [1..10^6])
  ]

-- The first three perform roughly the same, the last one is more than two times faster
-- (which is OK, I guess). Note that `take (10^6)` and `scan sum` commute
-- unlike `P.take (10^6)` and `P.scanl' (+) 0` -- this can be confusing.
-- I probably should rename `take` to `times` or something like that.
test14 :: IO ()
test14 = mapM_ print
  [ prefold (scan (take (10^6) sum) $ sum) [1..]
  , prefold (scan sum . take (10^6) $ sum) [1..]
  , prefold (take (10^6) . scan sum $ sum) [1..]
  , P.sum . P.take (10^6) $ P.scanl' (+) 0 [1..]
  ]

main = test14
