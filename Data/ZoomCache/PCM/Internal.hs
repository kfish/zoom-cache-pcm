{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM.Internal (
    -- * Classes
      ZoomPCMReadable(..)
    , ZoomPCMWritable(..)

    -- * Functions
    , updateSummaryPCM
) where

import Data.ZoomCache.Codec
import Data.ZoomCache.PCM.Types

----------------------------------------------------------------------
-- ZoomPCMReadable, ZoomPCMWritable

class ZoomReadable (PCM a) => ZoomPCMReadable a where
    pcmMin :: SummaryData (PCM a) -> a
    pcmMax :: SummaryData (PCM a) -> a
    pcmAvg :: SummaryData (PCM a) -> a
    pcmRMS :: SummaryData (PCM a) -> a

    pcmMkSummary :: a -> a -> a -> a -> SummaryData (PCM a)

class ZoomWritable (PCM a) => ZoomPCMWritable a where
    pcmWorkTime :: SummaryWork (PCM a) -> TimeStamp
    pcmWorkMin :: SummaryWork (PCM a) -> a
    pcmWorkMax :: SummaryWork (PCM a) -> a
    pcmWorkSum :: SummaryWork (PCM a) -> a
    pcmWorkSumSq :: SummaryWork (PCM a) -> a

    pcmMkSummaryWork :: TimeStamp -> a -> a -> a -> a -> SummaryWork (PCM a)

----------------------------------------------------------------------

updateSummaryPCM :: (Ord a, Num a,
                     ZoomPCMReadable a, ZoomPCMWritable a)
                 => TimeStamp -> PCM a
                 -> SummaryWork (PCM a)
                 -> SummaryWork (PCM a)
updateSummaryPCM t (PCM d) sw =
    pcmMkSummaryWork t (min (pcmWorkMin sw) d)
                       (max (pcmWorkMax sw) d)
                       ((pcmWorkSum sw) + (d * dur))
                       ((pcmWorkSumSq sw) + (d*d * dur))
    where
        !dur = fromIntegral $ (unTS t) - (unTS (pcmWorkTime sw))

