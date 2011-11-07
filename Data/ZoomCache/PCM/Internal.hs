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
) where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO)
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Monoid
import Data.Word

import Data.ZoomCache.Codec
import Data.ZoomCache.PCM.Types

----------------------------------------------------------------------

readSummaryPCM :: (I.Nullable s, LL.ListLike s Word8,
                   Functor m, MonadIO m,
                   ZoomPCM a)
               => Iteratee s m (SummaryData (PCM a))
readSummaryPCM = do
    [mn,mx] <- replicateM 2 (unPCM <$> readRaw)
    [avg,rms] <- replicateM 2 readDouble64be
    return (pcmMkSummary mn mx avg rms)
{-# INLINABLE readSummaryPCM #-}

fromSummaryPCM :: ZoomPCM a => SummaryData (PCM a) -> Builder
fromSummaryPCM s = mconcat $
    map pcmFromRaw [pcmMin s, pcmMax s] ++
    map fromDouble [pcmAvg s, pcmRMS s]
{-# INLINABLE fromSummaryPCM #-}

initSummaryPCMBounded :: (Bounded a, ZoomPCM a)
                      => TimeStamp -> SummaryWork (PCM a)
initSummaryPCMBounded entry = pcmMkSummaryWork entry maxBound minBound 0.0 0.0
{-# INLINEABLE initSummaryPCMBounded #-}

mkSummaryPCM :: ZoomPCM a
             => TimeStampDiff -> SummaryWork (PCM a)
             -> SummaryData (PCM a)
mkSummaryPCM (TSDiff dur) sw =
    pcmMkSummary (pcmWorkMin sw) (pcmWorkMax sw)
                 (pcmWorkSum sw / fromIntegral dur)
                 (sqrt $ (pcmWorkSumSq sw) / fromIntegral dur)
{-# INLINEABLE mkSummaryPCM #-}

appendSummaryPCM :: (Ord a, ZoomPCM a)
                 => TimeStampDiff -> SummaryData (PCM a)
                 -> TimeStampDiff -> SummaryData (PCM a)
                 -> SummaryData (PCM a)
appendSummaryPCM (TSDiff dur1) s1 (TSDiff dur2) s2 = pcmMkSummary
    (min (pcmMin s1) (pcmMin s2))
    (max (pcmMax s1) (pcmMax s2))
    (((pcmAvg s1 * fromIntegral dur1) + (pcmAvg s2 * fromIntegral dur2)) / fromIntegral durSum)
    (sqrt $ ((pcmRMS s1 * pcmRMS s1 * fromIntegral dur1) +
             (pcmRMS s2 * pcmRMS s2 * fromIntegral dur2)) /
            fromIntegral durSum)
    where
        !durSum = dur1 + dur2
{-# INLINEABLE appendSummaryPCM #-}

updateSummaryPCM :: (Ord a, Real a, ZoomPCM a)
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
