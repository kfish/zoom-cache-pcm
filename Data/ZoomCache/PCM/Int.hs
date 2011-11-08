{-# LANGUAGE BangPatterns #-}
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

The table below describes the encoding of SummaryData for 'PCM Int' and
'PCM Int32':

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (int32)                                                   | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (int32)                                                   | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Mean [DC Bias] (int32)                                        | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (int32)                                                   | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
@

Field encoding formats:

  @int32@:  32bit big endian
  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

-}
----------------------------------------------------------------------

module Data.ZoomCache.PCM.Int (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Int
import Data.Iteratee (Iteratee)
import Data.Word
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

{-# SPECIALIZE readSummaryPCM :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData (PCM Int)) #-}
{-# SPECIALIZE readSummaryPCM :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData (PCM Int)) #-}

instance ZoomWrite (PCM Int) where
    write = writeData

instance ZoomWrite (TimeStamp, (PCM Int)) where
    write = writeDataVBR

instance ZoomWritable (PCM Int) where
    data SummaryWork (PCM Int) = SummaryWorkPCMInt
        { swPCMIntTime  :: {-# UNPACK #-}!TimeStamp
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

instance ZoomPCM Int where
    pcmFromRaw = fromIntegral32be

    pcmMin = summaryIntMin
    pcmMax = summaryIntMax
    pcmAvg = summaryIntAvg
    pcmRMS = summaryIntRMS

    pcmWorkTime = swPCMIntTime
    pcmWorkMin = swPCMIntMin
    pcmWorkMax = swPCMIntMax
    pcmWorkSum = swPCMIntSum
    pcmWorkSumSq = swPCMIntSumSq

    pcmMkSummary = SummaryPCMInt
    pcmMkSummaryWork = SummaryWorkPCMInt

{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Int) -> Builder #-}
{-# SPECIALIZE initSummaryPCMBounded :: TimeStamp -> SummaryWork (PCM Int) #-}
{-# SPECIALIZE mkSummaryPCM :: TimeStampDiff -> SummaryWork (PCM Int) -> SummaryData (PCM Int) #-}
{-# SPECIALIZE appendSummaryPCM :: TimeStampDiff -> SummaryData (PCM Int) -> TimeStampDiff -> SummaryData (PCM Int) -> SummaryData (PCM Int) #-}
{-# SPECIALIZE updateSummaryPCM :: TimeStamp -> PCM Int -> SummaryWork (PCM Int) -> SummaryWork (PCM Int) #-}

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

{-# SPECIALIZE readSummaryPCM :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData (PCM Int32)) #-}
{-# SPECIALIZE readSummaryPCM :: (Functor m, MonadIO m) => Iteratee ByteString m (SummaryData (PCM Int32)) #-}

instance ZoomWrite (PCM Int32) where
    write = writeData

instance ZoomWrite (TimeStamp, (PCM Int32)) where
    write = writeDataVBR

instance ZoomWritable (PCM Int32) where
    data SummaryWork (PCM Int32) = SummaryWorkPCMInt32
        { swPCMInt32Time  :: {-# UNPACK #-}!TimeStamp
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

instance ZoomPCM Int32 where
    pcmFromRaw = fromIntegral32be

    pcmMin = summaryInt32Min
    pcmMax = summaryInt32Max
    pcmAvg = summaryInt32Avg
    pcmRMS = summaryInt32RMS

    pcmWorkTime = swPCMInt32Time
    pcmWorkMin = swPCMInt32Min
    pcmWorkMax = swPCMInt32Max
    pcmWorkSum = swPCMInt32Sum
    pcmWorkSumSq = swPCMInt32SumSq

    pcmMkSummary = SummaryPCMInt32
    pcmMkSummaryWork = SummaryWorkPCMInt32

{-# SPECIALIZE fromSummaryPCM :: SummaryData (PCM Int32) -> Builder #-}
{-# SPECIALIZE initSummaryPCMBounded :: TimeStamp -> SummaryWork (PCM Int32) #-}
{-# SPECIALIZE mkSummaryPCM :: TimeStampDiff -> SummaryWork (PCM Int32) -> SummaryData (PCM Int32) #-}
{-# SPECIALIZE appendSummaryPCM :: TimeStampDiff -> SummaryData (PCM Int32) -> TimeStampDiff -> SummaryData (PCM Int32) -> SummaryData (PCM Int32) #-}
{-# SPECIALIZE updateSummaryPCM :: TimeStamp -> PCM Int32 -> SummaryWork (PCM Int32) -> SummaryWork (PCM Int32) #-}

----------------------------------------------------------------------

prettyPacketPCMInt :: Show a => PCM a -> String
prettyPacketPCMInt = show . unPCM

prettySummaryPCMInt :: (PrintfArg a, ZoomPCM a)
                    => SummaryData (PCM a) -> String
prettySummaryPCMInt s = concat
    [ printf "\tmin: %d\tmax: %d\t" (pcmMin s) (pcmMax s)
    , printf "avg: %.3f\trms: %.3f" (pcmAvg s) (pcmRMS s)
    ]

