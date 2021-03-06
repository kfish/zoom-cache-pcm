{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM.Types (
    -- * PCM Type
      PCM(..)

    -- * Classes
    , ZoomPCM(..)
) where

import Blaze.ByteString.Builder
import Data.Typeable
import Data.ZoomCache.Codec

----------------------------------------------------------------------
-- PCM

data PCM a = PCM { unPCM :: !a }
    deriving (Typeable)

----------------------------------------------------------------------
-- ZoomPCM

class (Ord a, Real a, ZoomReadable (PCM a), ZoomWritable (PCM a)) => ZoomPCM a where
    pcmFromRaw :: a -> Builder

    pcmMin :: SummaryData (PCM a) -> a
    pcmMax :: SummaryData (PCM a) -> a
    pcmAvg :: SummaryData (PCM a) -> Double
    pcmRMS :: SummaryData (PCM a) -> Double

    pcmWorkSO    :: SummaryWork (PCM a) -> SampleOffset
    pcmWorkLast  :: SummaryWork (PCM a) -> a
    pcmWorkMin   :: SummaryWork (PCM a) -> a
    pcmWorkMax   :: SummaryWork (PCM a) -> a
    pcmWorkSum   :: SummaryWork (PCM a) -> Double
    pcmWorkSumSq :: SummaryWork (PCM a) -> Double

    pcmMkSummary :: a -> a -> Double -> Double -> SummaryData (PCM a)
    pcmMkSummaryWork :: SampleOffset -> a -> a -> a -> Double -> Double
                     -> SummaryWork (PCM a)

