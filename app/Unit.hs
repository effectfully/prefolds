{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification, TypeOperators #-}
module Unit where

import Lib
import Fold
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



perform :: (Fold a Identity [a] -> Fold a Identity b) -> [a] -> b
perform f = execute (f list)

suite :: Suite
suite = do
  label "map"  $ do
    perform (map (^2)) []     === ([] :: [Integer])
    perform (map (^2)) [1..3] === [1,4,9]
  label "take" $ do
    perform (take 0)  []                    === ([] :: [Integer])
    perform (take 1)  []                    === ([] :: [Integer])
    perform (take 0)  [1..]                 === []
    perform (take 3)  [1,2]                 === [1,2]
    perform (take 2)  [1..3]                === [1,2]
    perform (take 3)  [1..3]                === [1..3]
    perform (take 3)  [1..]                 === [1..3]
    perform (take 3) ([1,2,3] ++ undefined) === [1..3]
  label "filter" $ do
    perform (filter even) []      === []
    perform (filter even) [1]     === []
    perform (filter even) [1,3,5] === []
    perform (filter even) [1..5]  === [2,4]
  label "map-take-filter" $ do
    perform (map (* 3) . take 5 . filter even) [1..] === [6,12]
    perform (map (* 3) . filter even . take 5) [1..] === [6,12,18,24,30]
    perform (take 5 . map (* 3) . filter even) [1..] === [6,12]
    perform (take 5 . filter even . map (* 3)) [1..] === [6,12]
    perform (filter even . map (* 3) . take 5) [1..] === [6,12,18,24,30]
    perform (filter even . take 5 . map (* 3)) [1..] === [6,12,18,24,30]
  label "drop" $ do
    perform (drop 0) []     === ([] :: [Integer])
    perform (drop 1) []     === ([] :: [Integer])
    perform (drop 0) [1]    === [1]
    perform (drop 1) [1]    === []
    perform (drop 2) [1]    === []
    perform (drop 0) [1..4] === [1..4]
    perform (drop 1) [1..4] === [2..4]
    perform (drop 3) [1..4] === [4]
    perform (drop 4) [1..4] === []
    perform (drop 5) [1..4] === []
    perform (drop 9) [1..4] === []
  label "takeWhile" $ do
    perform (takeWhile (<= 5))  []                   === []
    perform (takeWhile (<= 5))  [1..4]               === [1..4]
    perform (takeWhile (<= 5))  [1..6]               === [1..5]
    perform (takeWhile (<= 5))  [5..]                === [5]
    perform (takeWhile (<= 5))  [6..]                === []
    perform (takeWhile (<= 5)) ([4..6] ++ undefined) === [4,5]
  label "dropWhile" $ do
    perform (dropWhile (<= 5)) []     === []
    perform (dropWhile (<= 5)) [1..4] === []
    perform (dropWhile (<= 5)) [1..6] === [6]
    perform (dropWhile (<= 5)) [3..9] === [6..9]
    perform (dropWhile (<= 5)) [5..9] === [6..9]
  label "scan" $ do
    label "basic" $ do
      perform (scan sum) []     === [0]
      perform (scan sum) [1]    === [0,1]
      perform (scan sum) [1..5] === [0,1,3,6,10,15]
    label "stop" $ do
      label "single" $ do
        perform (take 0 . scan sum) [1..] === [0]
        perform (scan sum . take 0) [1..] === []
        perform (take 5 . scan sum) [1..] === [0,1,3,6,10,15]
        perform (scan sum . take 5) [1..] === [0,1,3,6,10]
        perform (scan (take 5 sum)) [1..] === [0,1,3,6,10,15]
      label "multi" $ do
        perform (take 5 . scan sum . take 3) [1..] === [0,1,3]
        perform (take 3 . scan sum . take 5) [1..] === [0,1,3,6]
        perform (scan (take 5 sum) . take 3) [1..] === [0,1,3]
        perform (scan (take 3 sum) . take 5) [1..] === [0,1,3,6]
        perform (scan (take 5 sum) . take 4) [1..] === [0,1,3,6]
        perform (scan (take 4 sum) . take 5) [1..] === [0,1,3,6,10]
        perform (scan (take 5 sum) . take 5) [1..] === [0,1,3,6,10]
  let testGroupBy gby = do
        label "basic" $ do
          execute (gby (<) list list) [1]                     === [[1]]
          execute (gby (<) list list) [1,2]                   === [[1,2]]
          execute (gby (<) list list) [1,3,2]                 === [[1,3],[2]]
          execute (gby (<) list list) [2,1,3]                 === [[2],[1,3]]
          execute (gby (<) list list) [2,1,3,4,5]             === [[2],[1,3,4,5]]
          execute (gby (<) list list) [2,1,3,4,5,4,2,1,4,6,8] === [[2],[1,3,4,5],[4],[2],[1,4,6,8]]
          execute (gby (<) list list) [1..5]                  === [[1..5]]
        label "stop" $ do
          let xs = [7,1,2,3,4,3,5,9,2] ++ [1..]
          execute (gby (<) (take 0 list) $ take 0 list)  xs === []
          execute (gby (<) (take 0 list) $ take 1 list)  xs === [[]]
          execute (gby (<) (take 0 list) $ take 3 list)  xs === [[],[],[]]
          execute (gby (<) (take 1 list) $ take 0 list)  xs === []
          execute (gby (<) (take 3 list) $ take 0 list)  xs === []
          execute (gby (<) (take 1 list) $ take 1 list)  xs === [[7]]
          execute (gby (<) (take 2 list) $ take 3 list)  xs === [[7],[1,2],[3,5]]
          execute (gby (<) (take 3 list) $ take 2 list)  xs === [[7],[1,2,3]]
          execute (gby (<) (take 3 list) $ take 3 list)  xs === [[7],[1,2,3],[3,5,9]]
          execute (take 12 $ gby (<)  list         list) xs === [[7],[1,2,3,4],[3,5,9],[2],[1,2,3]]
          execute (take 12 $ gby (<) (take 2 list) list) xs === [[7],[1,2],[3,5],[2],[1,2]]
  label "groupBy" $ do
    label "empty" $ do
      execute (groupBy (<) sum  list) [] ===  []
      execute (groupBy (<) list list) [] === ([] :: [[Integer]])
    testGroupBy groupBy
  label "groupBy1" $ do
    label "empty" $ do
      execute (groupBy1 (<) sum  list) [] ===  [0]
      execute (groupBy1 (<) list list) [] === ([[]] :: [[Integer]])
    testGroupBy groupBy1
  label "inits" $ do
    label "basic" $ do
      execute (inits list list) []     === [[] :: [Integer]]
      execute (inits list list) [1]    === [[],[1]]
      execute (inits list list) [1..4] === [[],[1],[1,2],[1,2,3],[1,2,3,4]]
    label "stop" $ do
      label "finite" $ do
        execute (inits (take 0 list) (take 0 list)) [1,2] === []
        execute (inits (take 3 list) (take 0 list)) [1,2] === []
        execute (inits (take 0 list) (take 1 list)) [1,2] === [[]]
        execute (inits (take 0 list) (take 4 list)) [1,2] === [[],[],[]]
        execute (inits (take 2 list) (take 4 list)) [1,2] === [[],[1],[1,2]]
        execute (inits (take 4 list) (take 4 list)) [1,2] === [[],[1],[1,2]]
        execute (inits (take 3 list) (take 5 list)) [1,2] === [[],[1],[1,2]]
      label "infinite" $ do
        execute (inits (take 0 list) (take 0 list)) [1..] === []
        execute (inits (take 3 list) (take 0 list)) [1..] === []
        execute (inits (take 0 list) (take 1 list)) [1..] === [[]]
        execute (inits (take 0 list) (take 4 list)) [1..] === [[],[],[],[]]
        execute (inits (take 2 list) (take 4 list)) [1..] === [[],[1],[1,2],[1,2]]
        execute (inits (take 4 list) (take 4 list)) [1..] === [[],[1],[1,2],[1,2,3]]
        execute (inits (take 3 list) (take 5 list)) [1..] === [[],[1],[1,2],[1,2,3],[1,2,3]]
  label "compose" $ do
    label "parallel" $ do
      label "product" $ do
        execute ((,) <$> list        <*> list)        []     === ([],[] :: [Integer])
        execute ((,) <$> list        <*> list)        [1..4] === ([1..4],[1..4])
        execute ((,) <$> take 3 list <*> list)        [1..4] === ([1..3],[1..3])
        execute ((,) <$> list        <*> take 3 list) [1..4] === ([1..3],[1..3])
        execute ((,) <$> take 0 list <*> take 1 list) [1..]  === ([],[])
        execute ((,) <$> take 1 list <*> take 0 list) [1..]  === ([],[])
        execute ((,) <$> take 1 list <*> take 1 list) [1..]  === ([1],[1])
        execute ((,) <$> take 3 list <*> take 4 list) [1..]  === ([1..3],[1..3])
        execute ((,) <$> take 4 list <*> take 3 list) [1..]  === ([1..3],[1..3])
        execute ((,) <$> take 4 list <*> take 4 list) [1..]  === ([1..4],[1..4])
      label "sum" $ do
        execute ((,) <$> list        <+> list)        []     === ([],[] :: [Integer])
        execute ((,) <$> list        <+> list)        [1..4] === ([1..4],[1..4])
        execute ((,) <$> take 3 list <+> list)        [1..4] === ([1..3],[1..4])
        execute ((,) <$> list        <+> take 3 list) [1..4] === ([1..4],[1..3])
        execute ((,) <$> take 0 list <+> take 1 list) [1..]  === ([],[1])
        execute ((,) <$> take 1 list <+> take 0 list) [1..]  === ([1],[])
        execute ((,) <$> take 1 list <+> take 1 list) [1..]  === ([1],[1])
        execute ((,) <$> take 3 list <+> take 4 list) [1..]  === ([1..3],[1..4])
        execute ((,) <$> take 4 list <+> take 3 list) [1..]  === ([1..4],[1..3])
        execute ((,) <$> take 4 list <+> take 4 list) [1..]  === ([1..4],[1..4])
    label "sequential" $ do
      label "connect" $ do
        execute ((,) <$> list        </> list)        []     === ([],[] :: [Integer])
        execute ((,) <$> list        </> list)        [1..4] === ([1..4],[])
        execute ((,) <$> take 3 list </> list)        [1..4] === ([1..3],[4])
        execute ((,) <$> list        </> take 3 list) [1..4] === ([1..4],[])
        execute ((,) <$> take 0 list </> take 1 list) [1..]  === ([],[1])
        execute ((,) <$> take 1 list </> take 0 list) [1..]  === ([1],[])
        execute ((,) <$> take 1 list </> take 1 list) [1..]  === ([1],[2])
        execute ((,) <$> take 3 list </> take 4 list) [1..]  === ([1..3],[4..7])
        execute ((,) <$> take 4 list </> take 3 list) [1..]  === ([1..4],[5..7])
        execute ((,) <$> take 4 list </> take 4 list) [1..]  === ([1..4],[5..8])
      label "weld" $ do
        execute ((,) <$> list        <//> list)        []     === ([],[] :: [Integer])
        execute ((,) <$> list        <//> list)        [1..4] === ([1..4],[]) -- Sic.
        execute ((,) <$> take 3 list <//> list)        [1..4] === ([1..3],[3,4])
        execute ((,) <$> list        <//> take 3 list) [1..4] === ([1..4],[]) -- Sic.
        execute ((,) <$> take 0 list <//> take 1 list) [1..]  === ([],[1])
        execute ((,) <$> take 1 list <//> take 0 list) [1..]  === ([1],[])
        execute ((,) <$> take 1 list <//> take 1 list) [1..]  === ([1],[1])
        execute ((,) <$> take 3 list <//> take 4 list) [1..]  === ([1..3],[3..6])
        execute ((,) <$> take 4 list <//> take 3 list) [1..]  === ([1..4],[4..6])
        execute ((,) <$> take 4 list <//> take 4 list) [1..]  === ([1..4],[4..7])
  label "random" $ do
    perform (takeWhile (< 10) . dropWhile (<= 3) . filter even)    [1..] === [4,6,8]
    execute ((,) <$> take 4 list <+> (drop 2 . take 4) list)       [1..] === ([1..4],[3..6])
    perform (filter even . scan sum . take 6 . dropWhile (<= 10))  [1..] === [12,20,30]
    execute ((,,) <$> take 4 list <+> take 3 list <*> take 2 list) [1..] === ([1..2],[1..2],[1..2])
    execute ((,,) <$> take 4 list <+> take 3 list <*> take 5 list) [1..] === ([1..4],[1..3],[1..4])
    execute ((,,) <$> take 4 list <+> take 3 list </> take 2 list) [1..] === ([1..4],[1..3],[5..6])

checkSuite :: IO ()
checkSuite = putStrLn $ runSuite suite
