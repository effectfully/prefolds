{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Lib
import Fold
import Unit
import Bench

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
  sink3 = takeWhile (<= 10) list                        -- [7,8,9,10]
  total = length -- total number of processed elements is 11, since
                 -- `takeWhile (<= 10)` forced `11` before it stopped.

main = checkSuite >> benchSuite
