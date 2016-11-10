{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Lib as P
import Fold
import Unit
import Bench
import Data.Monoid
import qualified Prelude as P
import qualified Data.List as P
import Criterion.Main

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

-- 140 MB total memory in use.
fail1 :: IO ()
fail1 = print . uncurry (\xs ys -> P.sum xs + P.sum ys) . P.span (1 ==) $
          replicate (10^7) (1 :: Integer)

-- 2 MB total memory in use.
nofail1 :: IO ()
nofail1 = print . execute (span (+) (1 ==) sum sum) $ replicate (10^7) (1 :: Integer)

main = benchSuite
