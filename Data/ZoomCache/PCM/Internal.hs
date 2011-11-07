{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM.Internal (
    -- * Functions
      readSummaryPCM
    , initSummaryPCMBounded
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO)
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Word

import Data.ZoomCache.Codec
import Data.ZoomCache.PCM.Types

----------------------------------------------------------------------

readSummaryPCM :: (I.Nullable s, LL.ListLike s Word8,
                   Functor m, MonadIO m,
                   ZoomReadable (PCM a), ZoomPCMReadable a)
               => Iteratee s m (SummaryData (PCM a))
readSummaryPCM = do
    [mn,mx] <- replicateM 2 (unPCM <$> readRaw)
    [avg,rms] <- replicateM 2 readDouble64be
    return (pcmMkSummary mn mx avg rms)
{-# INLINABLE readSummaryPCM #-}

initSummaryPCMBounded :: (Bounded a, ZoomPCMWritable a)
                      => TimeStamp -> SummaryWork (PCM a)
initSummaryPCMBounded entry = pcmMkSummaryWork entry maxBound minBound 0.0 0.0
{-# INLINEABLE initSummaryPCMBounded #-}
