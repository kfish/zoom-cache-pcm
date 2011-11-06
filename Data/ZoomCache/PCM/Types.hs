{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM.Types (
    -- * PCM Type
      PCM(..)

    -- * Classes
    , ZoomPCMReadable(..)
    , ZoomPCMWritable(..)
) where

import Data.Typeable
import Data.ZoomCache.Codec

----------------------------------------------------------------------
-- PCM

data PCM a = PCM { unPCM :: !a }
    deriving (Typeable)

----------------------------------------------------------------------
-- ZoomPCMReadable, ZoomPCMWritable

class ZoomReadable (PCM a) => ZoomPCMReadable a where
    pcmMin :: SummaryData (PCM a) -> a
    pcmMax :: SummaryData (PCM a) -> a
    pcmAvg :: SummaryData (PCM a) -> Double
    pcmRMS :: SummaryData (PCM a) -> Double

    pcmMkSummary :: a -> a -> Double -> Double -> SummaryData (PCM a)

class ZoomWritable (PCM a) => ZoomPCMWritable a where
    pcmWorkTime :: SummaryWork (PCM a) -> TimeStamp
    pcmWorkMin :: SummaryWork (PCM a) -> a
    pcmWorkMax :: SummaryWork (PCM a) -> a
    pcmWorkSum :: SummaryWork (PCM a) -> Double
    pcmWorkSumSq :: SummaryWork (PCM a) -> Double

    pcmMkSummaryWork :: TimeStamp -> a -> a -> Double -> Double
                     -> SummaryWork (PCM a)

