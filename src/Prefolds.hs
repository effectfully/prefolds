{-# LANGUAGE FlexibleContexts #-}
module Prefolds
  ( module Lib
  , module Data.Strict.Drive
  , module Fold.Core
  , module Unfold.Core
  , module Prefolds
  ) where

import Lib hiding (foldM)
import Data.Strict.Tuple
import Data.Strict.Maybe
import Data.Strict.Drive
import Fold.Core
import Unfold.Core

pairingM :: Monad m => Fold a m (b -> c) -> Unfold a m b -> m c
pairingM (Fold g2 f2 a2) (Unfold g1 f1 a1) = go a1 a2 where
  go a1 a2 = driveTM (\(a1', a2') -> g1 a1' <**> g2 a2')
                     (\(a1', a2') -> do
                         Pair mx a1'' <- f1 a1'
                         go (return a1'') $ maybe' a2 (f2 a2') mx)
                     ((,) <$> DriveT a1 <&> a2)
{-# INLINEABLE pairingM #-}

pairingM_ :: Monad m => Fold a m c -> Unfold a m b -> m c
pairingM_ = pairingM . fmap const
{-# INLINEABLE pairingM_ #-}

pairing :: Fold a Identity (b -> c) -> Unfold a Identity b -> c
pairing = runIdentity .* pairingM
{-# INLINEABLE pairing #-}

pairing_ :: Fold a Identity c -> Unfold a Identity b -> c
pairing_ = runIdentity .* pairingM_
{-# INLINEABLE pairing_ #-}
