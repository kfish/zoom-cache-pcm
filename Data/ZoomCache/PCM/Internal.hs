{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM.Internal (
    -- * Functions
      readSummaryPCM
    , fromSummaryPCM
    , initSummaryPCMBounded
    , mkSummaryPCM
    , appendSummaryPCM
    , updateSummaryPCM
    , deltaDecodePCM
    , deltaEncodePCM
) where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import Data.Monoid

import Data.ZoomCache.Codec
import Data.ZoomCache.PCM.Types

----------------------------------------------------------------------

readSummaryPCM :: (Functor m, Monad m, ZoomPCM a)
               => Iteratee ByteString m (SummaryData (PCM a))
readSummaryPCM = do
    [mn,mx] <- replicateM 2 (unPCM <$> readRaw)
    [avg,rms] <- replicateM 2 readDouble64be
    return (pcmMkSummary mn mx avg rms)
{-# INLINABLE readSummaryPCM #-}

fromSummaryPCM :: ZoomPCM a
               => SummaryData (PCM a) -> Builder
fromSummaryPCM s = mconcat $
    map pcmFromRaw [pcmMin s, pcmMax s] ++
    map fromDouble [pcmAvg s, pcmRMS s]
{-# INLINABLE fromSummaryPCM #-}

initSummaryPCMBounded :: (Bounded a, ZoomPCM a)
                      => SampleOffset -> SummaryWork (PCM a)
initSummaryPCMBounded entry = pcmMkSummaryWork entry 0 maxBound minBound 0.0 0.0
{-# INLINEABLE initSummaryPCMBounded #-}

mkSummaryPCM :: ZoomPCM a
             => SampleOffsetDiff -> SummaryWork (PCM a)
             -> SummaryData (PCM a)
mkSummaryPCM (SODiff dur) sw =
    pcmMkSummary (pcmWorkMin sw) (pcmWorkMax sw)
                 (pcmWorkSum sw / fromIntegral dur)
                 (sqrt $ (pcmWorkSumSq sw) / fromIntegral dur)
{-# INLINEABLE mkSummaryPCM #-}

appendSummaryPCM :: ZoomPCM a
                 => SampleOffsetDiff -> SummaryData (PCM a)
                 -> SampleOffsetDiff -> SummaryData (PCM a)
                 -> SummaryData (PCM a)
appendSummaryPCM (SODiff dur1) s1 (SODiff dur2) s2 = pcmMkSummary
    (min (pcmMin s1) (pcmMin s2))
    (max (pcmMax s1) (pcmMax s2))
    (((pcmAvg s1 * fromIntegral dur1) + (pcmAvg s2 * fromIntegral dur2)) / fromIntegral durSum)
    (sqrt $ ((pcmRMS s1 * pcmRMS s1 * fromIntegral dur1) +
             (pcmRMS s2 * pcmRMS s2 * fromIntegral dur2)) /
            fromIntegral durSum)
    where
        !durSum = dur1 + dur2
{-# INLINEABLE appendSummaryPCM #-}

updateSummaryPCM :: ZoomPCM a
                 => SampleOffset -> PCM a
                 -> SummaryWork (PCM a)
                 -> SummaryWork (PCM a)
updateSummaryPCM t (PCM d) sw =
    pcmMkSummaryWork t d
                     (min (pcmWorkMin sw) d)
                     (max (pcmWorkMax sw) d)
                     ((pcmWorkSum sw) + realToFrac (d * dur))
                     ((pcmWorkSumSq sw) + realToFrac (d*d * dur))
    where
        !dur = fromIntegral $ (unSO t) - (unSO (pcmWorkSO sw))
{-# INLINEABLE updateSummaryPCM #-}

deltaDecodePCM :: ZoomPCM a => [PCM a] -> [PCM a]
deltaDecodePCM = map PCM . deltaDecode . map unPCM
{-# INLINE deltaDecodePCM #-}

deltaEncodePCM :: ZoomPCM a => SummaryWork (PCM a) -> PCM a -> PCM a
deltaEncodePCM sw (PCM d) = PCM (d - pcmWorkLast sw)
{-# INLINE deltaEncodePCM #-}
