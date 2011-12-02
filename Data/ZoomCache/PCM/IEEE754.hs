{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.PCM.Internal
import Data.ZoomCache.PCM.Types

----------------------------------------------------------------------
-- Float

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

    deltaDecodeRaw    = deltaDecodePCM

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryPCM :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData (PCM Float)) #-}
#endif

instance ZoomWrite (PCM Float) where
    write = writeData

instance ZoomWrite (SampleOffset, PCM Float) where
    write = writeDataVBR

instance ZoomWritable (PCM Float) where
    data SummaryWork (PCM Float) = SummaryWorkPCMFloat
        { swPCMFloatTime  :: {-# UNPACK #-}!SampleOffset
        , swPCMFloatLast  :: {-# UNPACK #-}!Float
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
    deltaEncodeRaw    = deltaEncodePCM

instance ZoomPCM Float where
    pcmFromRaw = fromFloat

    pcmMin = summaryPCMFloatMin
    pcmMax = summaryPCMFloatMax
    pcmAvg = summaryPCMFloatAvg
    pcmRMS = summaryPCMFloatRMS

    pcmWorkSO = swPCMFloatTime
    pcmWorkLast = swPCMFloatLast
    pcmWorkMin = swPCMFloatMin
    pcmWorkMax = swPCMFloatMax
    pcmWorkSum = swPCMFloatSum
    pcmWorkSumSq = swPCMFloatSumSq

    pcmMkSummary = SummaryPCMFloat
    pcmMkSummaryWork = SummaryWorkPCMFloat

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Float) -> Builder #-}
{-# SPECIALIZE mkSummaryPCM :: SampleOffsetDiff -> SummaryWork (PCM Float) -> SummaryData (PCM Float) #-}
{-# SPECIALIZE appendSummaryPCM :: SampleOffsetDiff -> SummaryData (PCM Float) -> SampleOffsetDiff -> SummaryData (PCM Float) -> SummaryData (PCM Float) #-}
{-# SPECIALIZE updateSummaryPCM :: SampleOffset -> PCM Float -> SummaryWork (PCM Float) -> SummaryWork (PCM Float) #-}
#endif

----------------------------------------------------------------------
-- Double

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

    deltaDecodeRaw    = deltaDecodePCM

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryPCM :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData (PCM Double)) #-}
#endif

instance ZoomWrite (PCM Double) where
    write = writeData

instance ZoomWrite (SampleOffset, PCM Double) where
    write = writeDataVBR

instance ZoomWritable (PCM Double) where
    data SummaryWork (PCM Double) = SummaryWorkPCMDouble
        { swPCMDoubleTime  :: {-# UNPACK #-}!SampleOffset
        , swPCMDoubleLast  :: {-# UNPACK #-}!Double
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
    deltaEncodeRaw    = deltaEncodePCM

instance ZoomPCM Double where
    pcmFromRaw = fromDouble

    pcmMin = summaryPCMDoubleMin
    pcmMax = summaryPCMDoubleMax
    pcmAvg = summaryPCMDoubleAvg
    pcmRMS = summaryPCMDoubleRMS

    pcmWorkSO = swPCMDoubleTime
    pcmWorkLast = swPCMDoubleLast
    pcmWorkMin = swPCMDoubleMin
    pcmWorkMax = swPCMDoubleMax
    pcmWorkSum = swPCMDoubleSum
    pcmWorkSumSq = swPCMDoubleSumSq

    pcmMkSummary = SummaryPCMDouble
    pcmMkSummaryWork = SummaryWorkPCMDouble

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Double) -> Builder #-}
{-# SPECIALIZE mkSummaryPCM :: SampleOffsetDiff -> SummaryWork (PCM Double) -> SummaryData (PCM Double) #-}
{-# SPECIALIZE appendSummaryPCM :: SampleOffsetDiff -> SummaryData (PCM Double) -> SampleOffsetDiff -> SummaryData (PCM Double) -> SummaryData (PCM Double) #-}
{-# SPECIALIZE updateSummaryPCM :: SampleOffset -> PCM Double -> SummaryWork (PCM Double) -> SummaryWork (PCM Double) #-}
#endif

----------------------------------------------------------------------
-- Helpers for float and double

prettyPacketPCMFloat :: PrintfArg a => PCM a -> String
prettyPacketPCMFloat = printf "%.3f" . unPCM

prettySummaryPCMFloat :: (PrintfArg a, ZoomPCM a)
                      => SummaryData (PCM a) -> String
prettySummaryPCMFloat s = concat
    [ printf "\tmin: %.3f\tmax: %.3f\t" (pcmMin s) (pcmMax s)
    , printf "avg: %.3f\trms: %.3f" (pcmAvg s) (pcmRMS s)
    ]

initSummaryPCMFloat :: (RealFloat a, ZoomPCM a)
                    => SampleOffset -> SummaryWork (PCM a)
initSummaryPCMFloat entry = pcmMkSummaryWork
    entry
    0.0
    floatMax
    (negate floatMax)
    0.0
    0.0
