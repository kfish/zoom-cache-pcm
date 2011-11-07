{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.PCM.IEEE754
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for PCM Audio of type Float and Double.
This module implements the interfaces documented in "Data.ZoomCache.Codec".

The table below describes the encoding of SummaryData for PCM.Float.

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (float)                                                   | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (float)                                                   | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Mean [DC Bias] (float)                                        | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (float)                                                   | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

The table below describes the encoding of SummaryData for PCM.Double.

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (double)                                                  | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (double)                                                  | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Mean [DC Bias] (double)                                       | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 56-59
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 60-63
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 64-67
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

Field encoding formats:

  @float@:  big-endian IEEE 754-2008 binary32 (IEEE 754-1985 single)
  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

-}
----------------------------------------------------------------------

module Data.ZoomCache.PCM.IEEE754 (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.PCM.Internal
import Data.ZoomCache.PCM.Types
-- import Numeric.FloatMinMax

----------------------------------------------------------------------
-- Read

instance ZoomReadable (PCM Float) where
    data SummaryData (PCM Float) = SummaryPCMFloat
        { summaryPCMFloatMin   :: {-# UNPACK #-}!Float
        , summaryPCMFloatMax   :: {-# UNPACK #-}!Float
        , summaryPCMFloatAvg   :: {-# UNPACK #-}!Double
        , summaryPCMFloatRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZPCMf32b"

    readRaw     = PCM <$> readFloat32be
    readSummary = readSummaryPCM

    prettyRaw         = prettyPacketPCMFloat
    prettySummaryData = prettySummaryPCMFloat

{-# SPECIALIZE readSummaryPCM :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData (PCM Float)) #-}
{-# SPECIALIZE readSummaryPCM :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData (PCM Float)) #-}

----------------------------------------------------------------------

instance ZoomReadable (PCM Double) where
    data SummaryData (PCM Double) = SummaryPCMDouble
        { summaryPCMDoubleMin   :: {-# UNPACK #-}!Double
        , summaryPCMDoubleMax   :: {-# UNPACK #-}!Double
        , summaryPCMDoubleAvg   :: {-# UNPACK #-}!Double
        , summaryPCMDoubleRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZPCMf64b"

    readRaw     = PCM <$> readDouble64be
    readSummary = readSummaryPCM

    prettyRaw         = prettyPacketPCMFloat
    prettySummaryData = prettySummaryPCMFloat

{-# SPECIALIZE readSummaryPCM :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData (PCM Double)) #-}
{-# SPECIALIZE readSummaryPCM :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData (PCM Double)) #-}

----------------------------------------------------------------------

prettyPacketPCMFloat :: PrintfArg a => PCM a -> String
prettyPacketPCMFloat = printf "%.3f" . unPCM

prettySummaryPCMFloat :: (PrintfArg a, ZoomPCM a)
                      => SummaryData (PCM a) -> String
prettySummaryPCMFloat s = concat
    [ printf "\tmin: %.3f\tmax: %.3f\t" (pcmMin s) (pcmMax s)
    , printf "avg: %.3f\trms: %.3f" (pcmAvg s) (pcmRMS s)
    ]

----------------------------------------------------------------------
-- Write

instance ZoomWrite (PCM Float) where
    write = writeData

instance ZoomWrite (TimeStamp, PCM Float) where
    write = writeDataVBR

instance ZoomWritable (PCM Float) where
    data SummaryWork (PCM Float) = SummaryWorkPCMFloat
        { swPCMFloatTime  :: {-# UNPACK #-}!TimeStamp
        , swPCMFloatMin   :: {-# UNPACK #-}!Float
        , swPCMFloatMax   :: {-# UNPACK #-}!Float
        , swPCMFloatSum   :: {-# UNPACK #-}!Double
        , swPCMFloatSumSq :: {-# UNPACK #-}!Double
        }
    fromRaw           = pcmFromRaw . unPCM
    fromSummaryData   = fromSummaryPCM

    initSummaryWork   = initSummaryPCMFloat
    toSummaryData     = mkSummaryPCM
    updateSummaryData = updateSummaryPCM
    appendSummaryData = appendSummaryPCM

instance ZoomPCM Float where
    pcmFromRaw = fromFloat

    pcmMin = summaryPCMFloatMin
    pcmMax = summaryPCMFloatMax
    pcmAvg = summaryPCMFloatAvg
    pcmRMS = summaryPCMFloatRMS

    pcmWorkTime = swPCMFloatTime
    pcmWorkMin = swPCMFloatMin
    pcmWorkMax = swPCMFloatMax
    pcmWorkSum = swPCMFloatSum
    pcmWorkSumSq = swPCMFloatSumSq

    pcmMkSummary = SummaryPCMFloat
    pcmMkSummaryWork = SummaryWorkPCMFloat

{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Float) -> Builder #-}
{-# SPECIALIZE mkSummaryPCM :: Double -> SummaryWork (PCM Float) -> SummaryData (PCM Float) #-}
{-# SPECIALIZE appendSummaryPCM :: Double -> SummaryData (PCM Float) -> Double -> SummaryData (PCM Float) -> SummaryData (PCM Float) #-}
{-# SPECIALIZE updateSummaryPCM :: TimeStamp -> PCM Float -> SummaryWork (PCM Float) -> SummaryWork (PCM Float) #-}

instance ZoomWrite (PCM Double) where
    write = writeData

instance ZoomWrite (TimeStamp, PCM Double) where
    write = writeDataVBR

instance ZoomWritable (PCM Double) where
    data SummaryWork (PCM Double) = SummaryWorkPCMDouble
        { swPCMDoubleTime  :: {-# UNPACK #-}!TimeStamp
        , swPCMDoubleMin   :: {-# UNPACK #-}!Double
        , swPCMDoubleMax   :: {-# UNPACK #-}!Double
        , swPCMDoubleSum   :: {-# UNPACK #-}!Double
        , swPCMDoubleSumSq :: {-# UNPACK #-}!Double
        }
    fromRaw           = pcmFromRaw . unPCM
    fromSummaryData   = fromSummaryPCM

    initSummaryWork   = initSummaryPCMFloat
    toSummaryData     = mkSummaryPCM
    updateSummaryData = updateSummaryPCM
    appendSummaryData = appendSummaryPCM

instance ZoomPCM Double where
    pcmFromRaw = fromDouble

    pcmMin = summaryPCMDoubleMin
    pcmMax = summaryPCMDoubleMax
    pcmAvg = summaryPCMDoubleAvg
    pcmRMS = summaryPCMDoubleRMS

    pcmWorkTime = swPCMDoubleTime
    pcmWorkMin = swPCMDoubleMin
    pcmWorkMax = swPCMDoubleMax
    pcmWorkSum = swPCMDoubleSum
    pcmWorkSumSq = swPCMDoubleSumSq

    pcmMkSummary = SummaryPCMDouble
    pcmMkSummaryWork = SummaryWorkPCMDouble

{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Double) -> Builder #-}
{-# SPECIALIZE mkSummaryPCM :: Double -> SummaryWork (PCM Double) -> SummaryData (PCM Double) #-}
{-# SPECIALIZE appendSummaryPCM :: Double -> SummaryData (PCM Double) -> Double -> SummaryData (PCM Double) -> SummaryData (PCM Double) #-}
{-# SPECIALIZE updateSummaryPCM :: TimeStamp -> PCM Double -> SummaryWork (PCM Double) -> SummaryWork (PCM Double) #-}

initSummaryPCMFloat :: (Fractional a, ZoomPCM a)
                    => TimeStamp -> SummaryWork (PCM a)
initSummaryPCMFloat entry = pcmMkSummaryWork
    entry
    1000.0 -- floatMax
    (-1000.0) -- negate floatMax
    0.0
    0.0
