{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification, NoImplicitPrelude #-}
module Unfold.Core where

import Lib hiding (foldM)
import Data.Strict.Tuple
import Data.Strict.Maybe
import Data.Strict.Drive
import qualified Lib
import qualified Unfold.Pure as Pure

data Unfold a m b = forall acc.
  Unfold (acc -> m b) (acc -> m (Pair (Maybe' a) (Drive acc))) (m (Drive acc))

fromPureUnfold :: Monad m => Pure.Unfold a -> Unfold a m ()
fromPureUnfold (Pure.Unfold f a) = Unfold (\_ -> return ()) (return . first Just' . f) (return a)
{-# INLINEABLE fromPureUnfold #-}

toUnfold :: Monad m => [a] -> Unfold a m ()
toUnfold = fromPureUnfold . Pure.toUnfold
{-# INLINEABLE toUnfold #-}

iterate :: Monad m => (a -> a) -> a -> Unfold a m ()
iterate = fromPureUnfold .* Pure.iterate
{-# INLINEABLE iterate #-}

repeat :: Monad m => a -> Unfold a m ()
repeat = fromPureUnfold . Pure.repeat
{-# INLINEABLE repeat #-}

enumFrom :: (Monad m, Enum a) => a -> Unfold a m ()
enumFrom = fromPureUnfold . Pure.enumFrom
{-# INLINEABLE enumFrom #-}
