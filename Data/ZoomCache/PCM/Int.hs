{-# LANGUAGE BangPatterns #-}
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

The table below describes the encoding of SummaryData for PCM.Int.

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Entry (int32)                                                 | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit (int32)                                                  | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (int32)                                                   | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (int32)                                                   | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (double)                                                  | 52-55
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

  @double@: big-endian IEEE 754-2008 binary64 (IEEE 754-1985 double)

-}
----------------------------------------------------------------------

module Data.ZoomCache.PCM.Int (
      SummaryData(..)
    , SummaryWork(..)
)where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO)
import Data.Iteratee (Iteratee)
import Data.Monoid
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.PCM.Types

----------------------------------------------------------------------
-- Read

instance ZoomReadable (PCM Int) where
    data SummaryData (PCM Int) = SummaryPCMInt
        { summaryIntEntry :: {-# UNPACK #-}!Int
        , summaryIntExit  :: {-# UNPACK #-}!Int
        , summaryIntMin   :: {-# UNPACK #-}!Int
        , summaryIntMax   :: {-# UNPACK #-}!Int
        , summaryIntAvg   :: {-# UNPACK #-}!Double
        , summaryIntRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZPCMi32b"

    readRaw     = PCM <$> readInt32be
    readSummary = readSummaryPCMInt

    prettyRaw         = prettyPacketPCMInt
    prettySummaryData = prettySummaryPCMInt

prettyPacketPCMInt :: PCM Int -> String
prettyPacketPCMInt = show . unPCM

readSummaryPCMInt :: (Functor m, MonadIO m)
                  => Iteratee [Word8] m (SummaryData (PCM Int))
readSummaryPCMInt = do
    [en,ex,mn,mx] <- replicateM 4 readInt32be
    [avg,rms] <- replicateM 2 readDouble64be
    return (SummaryPCMInt en ex mn mx avg rms)

prettySummaryPCMInt :: SummaryData (PCM Int) -> String
prettySummaryPCMInt SummaryPCMInt{..} = concat
    [ printf "\tentry: %d\texit: %df\tmin: %d\tmax: %d\t"
          summaryIntEntry summaryIntExit summaryIntMin summaryIntMax
    , printf "avg: %.3f\trms: %.3f" summaryIntAvg summaryIntRMS
    ]

{-
    typeOfSummaryData = typeOfSummaryPCMInt

typeOfSummaryPCMInt :: SummaryData (PCM Int) -> TypeRep
typeOfSummaryPCMInt _ = mkTyConApp tyCon [i,i,i,i]
    where
        tyCon = mkTyCon3 "zoom-cache" "Data.ZoomCache.Types" "SummaryPCMInt"
        i = typeOf (undefined :: Int)
-}

----------------------------------------------------------------------
-- Write

instance ZoomWrite (PCM Int) where
    write = writeData

instance ZoomWrite (TimeStamp, (PCM Int)) where
    write = writeDataVBR

instance ZoomWritable (PCM Int) where
    data SummaryWork (PCM Int) = SummaryWorkPCMInt
        { swPCMIntTime  :: {-# UNPACK #-}!TimeStamp
        , swPCMIntEntry :: {-# UNPACK #-}!Int
        , swPCMIntExit  :: {-# UNPACK #-}!Int
        , swPCMIntMin   :: {-# UNPACK #-}!Int
        , swPCMIntMax   :: {-# UNPACK #-}!Int
        , swPCMIntSum   :: {-# UNPACK #-}!Int
        , swPCMIntSumSq :: {-# UNPACK #-}!Double
        }

    fromRaw           = fromIntegral32be . unPCM
    fromSummaryData   = fromSummaryPCMInt

    initSummaryWork   = initSummaryPCMInt
    toSummaryData     = mkSummaryPCMInt
    updateSummaryData = updateSummaryPCMInt
    appendSummaryData = appendSummaryPCMInt

initSummaryPCMInt :: TimeStamp -> SummaryWork (PCM Int)
initSummaryPCMInt entry = SummaryWorkPCMInt
    { swPCMIntTime = entry
    , swPCMIntEntry = 0
    , swPCMIntExit = 0
    , swPCMIntMin = maxBound
    , swPCMIntMax = minBound
    , swPCMIntSum = 0
    , swPCMIntSumSq = 0
    }

mkSummaryPCMInt :: Double -> SummaryWork (PCM Int) -> SummaryData (PCM Int)
mkSummaryPCMInt dur SummaryWorkPCMInt{..} = SummaryPCMInt
    { summaryIntEntry = swPCMIntEntry
    , summaryIntExit = swPCMIntExit
    , summaryIntMin = swPCMIntMin
    , summaryIntMax = swPCMIntMax
    , summaryIntAvg = fromIntegral swPCMIntSum / dur
    , summaryIntRMS = sqrt $ swPCMIntSumSq / dur
    }

fromSummaryPCMInt :: SummaryData (PCM Int) -> Builder
fromSummaryPCMInt SummaryPCMInt{..} = mconcat $ map fromIntegral32be
    [ summaryIntEntry
    , summaryIntExit
    , summaryIntMin
    , summaryIntMax
    ] ++ map fromDouble
    [ summaryIntAvg
    , summaryIntRMS
    ]

updateSummaryPCMInt :: Int -> TimeStamp  -> PCM Int
                    -> SummaryWork (PCM Int)
                    -> SummaryWork (PCM Int)
updateSummaryPCMInt count t (PCM i) SummaryWorkPCMInt{..} = SummaryWorkPCMInt
    { swPCMIntTime = t
    , swPCMIntEntry = if count == 0 then i else swPCMIntEntry
    , swPCMIntExit = i
    , swPCMIntMin = min swPCMIntMin i
    , swPCMIntMax = max swPCMIntMax i
    , swPCMIntSum = swPCMIntSum + (i * dur)
    , swPCMIntSumSq = swPCMIntSumSq + fromIntegral (i*i * dur)
    }
    where
        !dur = fromIntegral $ (unTS t) - (unTS swPCMIntTime)

appendSummaryPCMInt :: Double -> SummaryData (PCM Int)
                 -> Double -> SummaryData (PCM Int)
                 -> SummaryData (PCM Int)
appendSummaryPCMInt dur1 s1 dur2 s2 = SummaryPCMInt
    { summaryIntEntry = summaryIntEntry s1
    , summaryIntExit = summaryIntExit s2
    , summaryIntMin = min (summaryIntMin s1) (summaryIntMin s2)
    , summaryIntMax = max (summaryIntMax s1) (summaryIntMax s2)
    , summaryIntAvg = ((summaryIntAvg s1 * dur1) +
                       (summaryIntAvg s2 * dur2)) /
                      durSum
    , summaryIntRMS = sqrt $ ((summaryIntRMS s1 * summaryIntRMS s1 * dur1) +
                              (summaryIntRMS s2 * summaryIntRMS s2 * dur2)) /
                             durSum
    }
    where
        !durSum = dur1 + dur2

