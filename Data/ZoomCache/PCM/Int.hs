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
   Module      : Data.ZoomCache.PCM.Int
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Default codec implementation for PCM Audio of type Int. This module
implements the interfaces documented in "Data.ZoomCache.Codec".

The table below describes the encoding of SummaryData for 'PCM Int16':

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (int16)                     | Max (int16)                 | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Mean [DC Bias] (double)                                       | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

The table below describes the encoding of SummaryData for 'PCM Int' and
'PCM Int32':

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (int32)                                                   | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (int32)                                                   | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Mean [DC Bias] (double)                                       | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 56-59
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

The table below describes the encoding of SummaryData for 'PCM Int64':

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (int64)                                                   | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (int64)                                                   | 44-47
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

  @int32@:  32bit big endian

  @int64@:  64bit big endian

  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

-}
----------------------------------------------------------------------

module Data.ZoomCache.PCM.Int (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Int
import Data.Iteratee (Iteratee)
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.PCM.Internal
import Data.ZoomCache.PCM.Types

----------------------------------------------------------------------
-- Int

instance ZoomReadable (PCM Int) where
    data SummaryData (PCM Int) = SummaryPCMInt
        { summaryIntMin   :: {-# UNPACK #-}!Int
        , summaryIntMax   :: {-# UNPACK #-}!Int
        , summaryIntAvg   :: {-# UNPACK #-}!Double
        , summaryIntRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZPCMi32b"

    readRaw     = PCM <$> readInt32be
    readSummary = readSummaryPCM

    prettyRaw         = prettyPacketPCMInt
    prettySummaryData = prettySummaryPCMInt

    deltaDecodeRaw = deltaDecodePCM

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryPCM :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData (PCM Int)) #-}
#endif

instance ZoomWrite (PCM Int) where
    write = writeData

instance ZoomWrite (SampleOffset, (PCM Int)) where
    write = writeDataVBR

instance ZoomWritable (PCM Int) where
    data SummaryWork (PCM Int) = SummaryWorkPCMInt
        { swPCMIntTime  :: {-# UNPACK #-}!SampleOffset
        , swPCMIntLast  :: {-# UNPACK #-}!Int
        , swPCMIntMin   :: {-# UNPACK #-}!Int
        , swPCMIntMax   :: {-# UNPACK #-}!Int
        , swPCMIntSum   :: {-# UNPACK #-}!Double
        , swPCMIntSumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = pcmFromRaw . unPCM
    fromSummaryData   = fromSummaryPCM

    initSummaryWork   = initSummaryPCMBounded
    toSummaryData     = mkSummaryPCM
    updateSummaryData = updateSummaryPCM
    appendSummaryData = appendSummaryPCM
    deltaEncodeRaw    = deltaEncodePCM

instance ZoomPCM Int where
    pcmFromRaw = fromIntegral32be

    pcmMin = summaryIntMin
    pcmMax = summaryIntMax
    pcmAvg = summaryIntAvg
    pcmRMS = summaryIntRMS

    pcmWorkSO = swPCMIntTime
    pcmWorkLast = swPCMIntLast
    pcmWorkMin = swPCMIntMin
    pcmWorkMax = swPCMIntMax
    pcmWorkSum = swPCMIntSum
    pcmWorkSumSq = swPCMIntSumSq

    pcmMkSummary = SummaryPCMInt
    pcmMkSummaryWork = SummaryWorkPCMInt

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Int) -> Builder #-}
{-# SPECIALIZE initSummaryPCMBounded :: SampleOffset -> SummaryWork (PCM Int) #-}
{-# SPECIALIZE mkSummaryPCM :: SampleOffsetDiff -> SummaryWork (PCM Int) -> SummaryData (PCM Int) #-}
{-# SPECIALIZE appendSummaryPCM :: SampleOffsetDiff -> SummaryData (PCM Int) -> SampleOffsetDiff -> SummaryData (PCM Int) -> SummaryData (PCM Int) #-}
{-# SPECIALIZE updateSummaryPCM :: SampleOffset -> PCM Int -> SummaryWork (PCM Int) -> SummaryWork (PCM Int) #-}
#endif

----------------------------------------------------------------------
-- Int8

instance ZoomReadable (PCM Int8) where
    data SummaryData (PCM Int8) = SummaryPCMInt8
        { summaryInt8Min   :: {-# UNPACK #-}!Int8
        , summaryInt8Max   :: {-# UNPACK #-}!Int8
        , summaryInt8Avg   :: {-# UNPACK #-}!Double
        , summaryInt8RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZPCMiS8b"

    readRaw     = PCM <$> readInt8
    readSummary = readSummaryPCM

    prettyRaw         = prettyPacketPCMInt
    prettySummaryData = prettySummaryPCMInt

    deltaDecodeRaw = deltaDecodePCM

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryPCM :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData (PCM Int8)) #-}
#endif

instance ZoomWrite (PCM Int8) where
    write = writeData

instance ZoomWrite (SampleOffset, (PCM Int8)) where
    write = writeDataVBR

instance ZoomWritable (PCM Int8) where
    data SummaryWork (PCM Int8) = SummaryWorkPCMInt8
        { swPCMInt8Time  :: {-# UNPACK #-}!SampleOffset
        , swPCMInt8Last  :: {-# UNPACK #-}!Int8
        , swPCMInt8Min   :: {-# UNPACK #-}!Int8
        , swPCMInt8Max   :: {-# UNPACK #-}!Int8
        , swPCMInt8Sum   :: {-# UNPACK #-}!Double
        , swPCMInt8SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = pcmFromRaw . unPCM
    fromSummaryData   = fromSummaryPCM

    initSummaryWork   = initSummaryPCMBounded
    toSummaryData     = mkSummaryPCM
    updateSummaryData = updateSummaryPCM
    appendSummaryData = appendSummaryPCM
    deltaEncodeRaw    = deltaEncodePCM

instance ZoomPCM Int8 where
    pcmFromRaw = fromInt8

    pcmMin = summaryInt8Min
    pcmMax = summaryInt8Max
    pcmAvg = summaryInt8Avg
    pcmRMS = summaryInt8RMS

    pcmWorkSO = swPCMInt8Time
    pcmWorkLast = swPCMInt8Last
    pcmWorkMin = swPCMInt8Min
    pcmWorkMax = swPCMInt8Max
    pcmWorkSum = swPCMInt8Sum
    pcmWorkSumSq = swPCMInt8SumSq

    pcmMkSummary = SummaryPCMInt8
    pcmMkSummaryWork = SummaryWorkPCMInt8

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Int8) -> Builder #-}
{-# SPECIALIZE initSummaryPCMBounded :: SampleOffset -> SummaryWork (PCM Int8) #-}
{-# SPECIALIZE mkSummaryPCM :: SampleOffsetDiff -> SummaryWork (PCM Int8) -> SummaryData (PCM Int8) #-}
{-# SPECIALIZE appendSummaryPCM :: SampleOffsetDiff -> SummaryData (PCM Int8) -> SampleOffsetDiff -> SummaryData (PCM Int8) -> SummaryData (PCM Int8) #-}
{-# SPECIALIZE updateSummaryPCM :: SampleOffset -> PCM Int8 -> SummaryWork (PCM Int8) -> SummaryWork (PCM Int8) #-}
#endif

----------------------------------------------------------------------
-- Int16

instance ZoomReadable (PCM Int16) where
    data SummaryData (PCM Int16) = SummaryPCMInt16
        { summaryInt16Min   :: {-# UNPACK #-}!Int16
        , summaryInt16Max   :: {-# UNPACK #-}!Int16
        , summaryInt16Avg   :: {-# UNPACK #-}!Double
        , summaryInt16RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZPCMi16b"

    readRaw     = PCM <$> readInt16be
    readSummary = readSummaryPCM

    prettyRaw         = prettyPacketPCMInt
    prettySummaryData = prettySummaryPCMInt

    deltaDecodeRaw = deltaDecodePCM

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryPCM :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData (PCM Int16)) #-}
#endif

instance ZoomWrite (PCM Int16) where
    write = writeData

instance ZoomWrite (SampleOffset, (PCM Int16)) where
    write = writeDataVBR

instance ZoomWritable (PCM Int16) where
    data SummaryWork (PCM Int16) = SummaryWorkPCMInt16
        { swPCMInt16Time  :: {-# UNPACK #-}!SampleOffset
        , swPCMInt16Last  :: {-# UNPACK #-}!Int16
        , swPCMInt16Min   :: {-# UNPACK #-}!Int16
        , swPCMInt16Max   :: {-# UNPACK #-}!Int16
        , swPCMInt16Sum   :: {-# UNPACK #-}!Double
        , swPCMInt16SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = pcmFromRaw . unPCM
    fromSummaryData   = fromSummaryPCM

    initSummaryWork   = initSummaryPCMBounded
    toSummaryData     = mkSummaryPCM
    updateSummaryData = updateSummaryPCM
    appendSummaryData = appendSummaryPCM
    deltaEncodeRaw    = deltaEncodePCM

instance ZoomPCM Int16 where
    pcmFromRaw = fromInt16be

    pcmMin = summaryInt16Min
    pcmMax = summaryInt16Max
    pcmAvg = summaryInt16Avg
    pcmRMS = summaryInt16RMS

    pcmWorkSO = swPCMInt16Time
    pcmWorkLast = swPCMInt16Last
    pcmWorkMin = swPCMInt16Min
    pcmWorkMax = swPCMInt16Max
    pcmWorkSum = swPCMInt16Sum
    pcmWorkSumSq = swPCMInt16SumSq

    pcmMkSummary = SummaryPCMInt16
    pcmMkSummaryWork = SummaryWorkPCMInt16

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Int16) -> Builder #-}
{-# SPECIALIZE initSummaryPCMBounded :: SampleOffset -> SummaryWork (PCM Int16) #-}
{-# SPECIALIZE mkSummaryPCM :: SampleOffsetDiff -> SummaryWork (PCM Int16) -> SummaryData (PCM Int16) #-}
{-# SPECIALIZE appendSummaryPCM :: SampleOffsetDiff -> SummaryData (PCM Int16) -> SampleOffsetDiff -> SummaryData (PCM Int16) -> SummaryData (PCM Int16) #-}
{-# SPECIALIZE updateSummaryPCM :: SampleOffset -> PCM Int16 -> SummaryWork (PCM Int16) -> SummaryWork (PCM Int16) #-}
#endif

----------------------------------------------------------------------
-- Int32

instance ZoomReadable (PCM Int32) where
    data SummaryData (PCM Int32) = SummaryPCMInt32
        { summaryInt32Min   :: {-# UNPACK #-}!Int32
        , summaryInt32Max   :: {-# UNPACK #-}!Int32
        , summaryInt32Avg   :: {-# UNPACK #-}!Double
        , summaryInt32RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZPCMi32b"

    readRaw     = PCM <$> readInt32be
    readSummary = readSummaryPCM

    prettyRaw         = prettyPacketPCMInt
    prettySummaryData = prettySummaryPCMInt

    deltaDecodeRaw = deltaDecodePCM

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryPCM :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData (PCM Int32)) #-}
#endif

instance ZoomWrite (PCM Int32) where
    write = writeData

instance ZoomWrite (SampleOffset, (PCM Int32)) where
    write = writeDataVBR

instance ZoomWritable (PCM Int32) where
    data SummaryWork (PCM Int32) = SummaryWorkPCMInt32
        { swPCMInt32Time  :: {-# UNPACK #-}!SampleOffset
        , swPCMInt32Last  :: {-# UNPACK #-}!Int32
        , swPCMInt32Min   :: {-# UNPACK #-}!Int32
        , swPCMInt32Max   :: {-# UNPACK #-}!Int32
        , swPCMInt32Sum   :: {-# UNPACK #-}!Double
        , swPCMInt32SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = pcmFromRaw . unPCM
    fromSummaryData   = fromSummaryPCM

    initSummaryWork   = initSummaryPCMBounded
    toSummaryData     = mkSummaryPCM
    updateSummaryData = updateSummaryPCM
    appendSummaryData = appendSummaryPCM
    deltaEncodeRaw    = deltaEncodePCM

instance ZoomPCM Int32 where
    pcmFromRaw = fromIntegral32be

    pcmMin = summaryInt32Min
    pcmMax = summaryInt32Max
    pcmAvg = summaryInt32Avg
    pcmRMS = summaryInt32RMS

    pcmWorkSO = swPCMInt32Time
    pcmWorkLast = swPCMInt32Last
    pcmWorkMin = swPCMInt32Min
    pcmWorkMax = swPCMInt32Max
    pcmWorkSum = swPCMInt32Sum
    pcmWorkSumSq = swPCMInt32SumSq

    pcmMkSummary = SummaryPCMInt32
    pcmMkSummaryWork = SummaryWorkPCMInt32

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Int32) -> Builder #-}
{-# SPECIALIZE initSummaryPCMBounded :: SampleOffset -> SummaryWork (PCM Int32) #-}
{-# SPECIALIZE mkSummaryPCM :: SampleOffsetDiff -> SummaryWork (PCM Int32) -> SummaryData (PCM Int32) #-}
{-# SPECIALIZE appendSummaryPCM :: SampleOffsetDiff -> SummaryData (PCM Int32) -> SampleOffsetDiff -> SummaryData (PCM Int32) -> SummaryData (PCM Int32) #-}
{-# SPECIALIZE updateSummaryPCM :: SampleOffset -> PCM Int32 -> SummaryWork (PCM Int32) -> SummaryWork (PCM Int32) #-}
#endif

----------------------------------------------------------------------
-- Int64

instance ZoomReadable (PCM Int64) where
    data SummaryData (PCM Int64) = SummaryPCMInt64
        { summaryInt64Min   :: {-# UNPACK #-}!Int64
        , summaryInt64Max   :: {-# UNPACK #-}!Int64
        , summaryInt64Avg   :: {-# UNPACK #-}!Double
        , summaryInt64RMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZPCMi64b"

    readRaw     = PCM <$> readInt64be
    readSummary = readSummaryPCM

    prettyRaw         = prettyPacketPCMInt
    prettySummaryData = prettySummaryPCMInt

    deltaDecodeRaw = deltaDecodePCM

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE readSummaryPCM :: (Functor m, Monad m) => Iteratee ByteString m (SummaryData (PCM Int64)) #-}
#endif

instance ZoomWrite (PCM Int64) where
    write = writeData

instance ZoomWrite (SampleOffset, (PCM Int64)) where
    write = writeDataVBR

instance ZoomWritable (PCM Int64) where
    data SummaryWork (PCM Int64) = SummaryWorkPCMInt64
        { swPCMInt64Time  :: {-# UNPACK #-}!SampleOffset
        , swPCMInt64Last  :: {-# UNPACK #-}!Int64
        , swPCMInt64Min   :: {-# UNPACK #-}!Int64
        , swPCMInt64Max   :: {-# UNPACK #-}!Int64
        , swPCMInt64Sum   :: {-# UNPACK #-}!Double
        , swPCMInt64SumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = pcmFromRaw . unPCM
    fromSummaryData   = fromSummaryPCM

    initSummaryWork   = initSummaryPCMBounded
    toSummaryData     = mkSummaryPCM
    updateSummaryData = updateSummaryPCM
    appendSummaryData = appendSummaryPCM
    deltaEncodeRaw    = deltaEncodePCM

instance ZoomPCM Int64 where
    pcmFromRaw = fromInt64be

    pcmMin = summaryInt64Min
    pcmMax = summaryInt64Max
    pcmAvg = summaryInt64Avg
    pcmRMS = summaryInt64RMS

    pcmWorkSO = swPCMInt64Time
    pcmWorkLast = swPCMInt64Last
    pcmWorkMin = swPCMInt64Min
    pcmWorkMax = swPCMInt64Max
    pcmWorkSum = swPCMInt64Sum
    pcmWorkSumSq = swPCMInt64SumSq

    pcmMkSummary = SummaryPCMInt64
    pcmMkSummaryWork = SummaryWorkPCMInt64

#if __GLASGOW_HASKELL__ >= 702
{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Int64) -> Builder #-}
{-# SPECIALIZE initSummaryPCMBounded :: SampleOffset -> SummaryWork (PCM Int64) #-}
{-# SPECIALIZE mkSummaryPCM :: SampleOffsetDiff -> SummaryWork (PCM Int64) -> SummaryData (PCM Int64) #-}
{-# SPECIALIZE appendSummaryPCM :: SampleOffsetDiff -> SummaryData (PCM Int64) -> SampleOffsetDiff -> SummaryData (PCM Int64) -> SummaryData (PCM Int64) #-}
{-# SPECIALIZE updateSummaryPCM :: SampleOffset -> PCM Int64 -> SummaryWork (PCM Int64) -> SummaryWork (PCM Int64) #-}
#endif

----------------------------------------------------------------------

prettyPacketPCMInt :: Show a => PCM a -> String
prettyPacketPCMInt = show . unPCM

prettySummaryPCMInt :: (PrintfArg a, ZoomPCM a)
                    => SummaryData (PCM a) -> String
prettySummaryPCMInt s = concat
    [ printf "\tmin: %d\tmax: %d\t" (pcmMin s) (pcmMax s)
    , printf "avg: %.3f\trms: %.3f" (pcmAvg s) (pcmRMS s)
    ]

