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

class (ZoomReadable (PCM a), ZoomWritable (PCM a)) => ZoomPCM a where
    pcmFromRaw :: a -> Builder

    pcmMin :: SummaryData (PCM a) -> a
    pcmMax :: SummaryData (PCM a) -> a
    pcmAvg :: SummaryData (PCM a) -> Double
    pcmRMS :: SummaryData (PCM a) -> Double

    pcmWorkTime :: SummaryWork (PCM a) -> TimeStamp
    pcmWorkMin :: SummaryWork (PCM a) -> a
    pcmWorkMax :: SummaryWork (PCM a) -> a
    pcmWorkSum :: SummaryWork (PCM a) -> Double
    pcmWorkSumSq :: SummaryWork (PCM a) -> Double

    pcmMkSummary :: a -> a -> Double -> Double -> SummaryData (PCM a)
    pcmMkSummaryWork :: TimeStamp -> a -> a -> Double -> Double
                     -> SummaryWork (PCM a)

