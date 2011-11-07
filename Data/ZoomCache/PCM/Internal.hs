{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM.Internal (
    -- * Functions
      readSummaryPCM
    , initSummaryPCMBounded
    , appendSummaryPCM
    , updateSummaryPCM
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

appendSummaryPCM :: (Ord a, ZoomPCMReadable a)
                 => Double -> SummaryData (PCM a)
                 -> Double -> SummaryData (PCM a)
                 -> SummaryData (PCM a)
appendSummaryPCM dur1 s1 dur2 s2 = pcmMkSummary
    (min (pcmMin s1) (pcmMin s2))
    (max (pcmMax s1) (pcmMax s2))
    (((pcmAvg s1 * dur1) + (pcmAvg s2 * dur2)) / durSum)
    (sqrt $ ((pcmRMS s1 * pcmRMS s1 * dur1) +
             (pcmRMS s2 * pcmRMS s2 * dur2)) /
            durSum)
    where
        !durSum = dur1 + dur2
{-# INLINEABLE appendSummaryPCM #-}

updateSummaryPCM :: (Ord a, Real a,
                     ZoomPCMReadable a, ZoomPCMWritable a)
                 => TimeStamp -> PCM a
                 -> SummaryWork (PCM a)
                 -> SummaryWork (PCM a)
updateSummaryPCM t (PCM d) sw =
    pcmMkSummaryWork t (min (pcmWorkMin sw) d)
                       (max (pcmWorkMax sw) d)
                       ((pcmWorkSum sw) + realToFrac (d * dur))
                       ((pcmWorkSumSq sw) + realToFrac (d*d * dur))
    where
        !dur = fromIntegral $ (unTS t) - (unTS (pcmWorkTime sw))
{-# INLINEABLE updateSummaryPCM #-}
