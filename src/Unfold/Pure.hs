{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification, NoImplicitPrelude #-}
module Unfold.Pure where

import Lib hiding (foldM)
import Data.Strict.Tuple
import Data.Strict.Maybe
import Data.Strict.Drive
import qualified Prelude as P

data Unfold a = forall acc. Unfold (acc -> Pair a (Drive acc)) (Drive acc)

toUnfold :: [a] -> Unfold a
toUnfold xs = Unfold (\(x:xs) -> Pair x (finish xs)) (finish xs) where
  finish = haltWhen P.null
{-# INLINEABLE toUnfold #-}

iterate :: (a -> a) -> a -> Unfold a
iterate f x = Unfold (ap Pair More . f) (More x)
{-# INLINEABLE iterate #-}

repeat :: a -> Unfold a
repeat = iterate id
{-# INLINEABLE repeat #-}

enumFrom :: Enum a => a -> Unfold a
enumFrom = iterate succ
{-# INLINEABLE enumFrom #-}

{-enumFromTo :: Enum a => a -> a -> Unfold a
enumFromTo n m = Unfold (ap Pair More . f) (More n) where
  finish = haltWhen 
{-# INLINEABLE enumFrom #-}-}

{-enumFrom :: Enum a => a -> Unfold a
enumFrom = iterate succ
{-# INLINEABLE enumFrom #-}-}
