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

The table below describes the encoding of SummaryData for PCM.Int.

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
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString as B
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Monoid
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.PCM.Types

----------------------------------------------------------------------
-- Read

instance ZoomReadable (PCM Int) where
    data SummaryData (PCM Int) = SummaryPCMInt
        { summaryIntMin   :: {-# UNPACK #-}!Int
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

readSummaryPCMInt :: (I.Nullable s, LL.ListLike s Word8, Functor m, MonadIO m)
                  => Iteratee s m (SummaryData (PCM Int))
readSummaryPCMInt = do
    [mn,mx]   <- replicateM 2 readInt32be
    [avg,rms] <- replicateM 2 readDouble64be
    return (SummaryPCMInt mn mx avg rms)
{-# SPECIALIZE INLINE readSummaryPCMInt :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData (PCM Int)) #-}
{-# SPECIALIZE INLINE readSummaryPCMInt :: (Functor m, MonadIO m) => Iteratee B.ByteString m (SummaryData (PCM Int)) #-}

prettySummaryPCMInt :: SummaryData (PCM Int) -> String
prettySummaryPCMInt SummaryPCMInt{..} = concat
    [ printf "\tmin: %d\tmax: %d\t" summaryIntMin summaryIntMax
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
    , swPCMIntMin = maxBound
    , swPCMIntMax = minBound
    , swPCMIntSum = 0
    , swPCMIntSumSq = 0
    }

mkSummaryPCMInt :: Double -> SummaryWork (PCM Int) -> SummaryData (PCM Int)
mkSummaryPCMInt dur SummaryWorkPCMInt{..} = SummaryPCMInt
    { summaryIntMin = swPCMIntMin
    , summaryIntMax = swPCMIntMax
    , summaryIntAvg = fromIntegral swPCMIntSum / dur
    , summaryIntRMS = sqrt $ swPCMIntSumSq / dur
    }

fromSummaryPCMInt :: SummaryData (PCM Int) -> Builder
fromSummaryPCMInt SummaryPCMInt{..} = mconcat $ map fromIntegral32be
    [ summaryIntMin
    , summaryIntMax
    ] ++ map fromDouble
    [ summaryIntAvg
    , summaryIntRMS
    ]

updateSummaryPCMInt :: TimeStamp -> PCM Int
                    -> SummaryWork (PCM Int)
                    -> SummaryWork (PCM Int)
updateSummaryPCMInt t (PCM i) SummaryWorkPCMInt{..} = SummaryWorkPCMInt
    { swPCMIntTime = t
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
    { summaryIntMin = min (summaryIntMin s1) (summaryIntMin s2)
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

