{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.PCM.Double
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

module Data.ZoomCache.PCM.Double (
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
-- import Numeric.FloatMinMax

----------------------------------------------------------------------
-- ZoomPCMReadable

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
-- Read

instance ZoomPCMReadable Float where
    pcmMin = summaryPCMFloatMin
    pcmMax = summaryPCMFloatMax
    pcmAvg = summaryPCMFloatAvg
    pcmRMS = summaryPCMFloatRMS

    pcmMkSummary = SummaryPCMFloat

instance ZoomReadable (PCM Float) where
    data SummaryData (PCM Float) = SummaryPCMFloat
        { summaryPCMFloatMin   :: {-# UNPACK #-}!Float
        , summaryPCMFloatMax   :: {-# UNPACK #-}!Float
        , summaryPCMFloatAvg   :: {-# UNPACK #-}!Float
        , summaryPCMFloatRMS   :: {-# UNPACK #-}!Float
        }

    trackIdentifier = const "ZPCMf32b"

    readRaw     = PCM <$> readFloat32be
    readSummary = readSummaryPCMFloat

    prettyRaw         = prettyPacketPCMFloat
    prettySummaryData = prettySummaryPCMFloat

instance ZoomPCMReadable Double where
    pcmMin = summaryPCMDoubleMin
    pcmMax = summaryPCMDoubleMax
    pcmAvg = summaryPCMDoubleAvg
    pcmRMS = summaryPCMDoubleRMS
    pcmMkSummary = SummaryPCMDouble

instance ZoomReadable (PCM Double) where
    data SummaryData (PCM Double) = SummaryPCMDouble
        { summaryPCMDoubleMin   :: {-# UNPACK #-}!Double
        , summaryPCMDoubleMax   :: {-# UNPACK #-}!Double
        , summaryPCMDoubleAvg   :: {-# UNPACK #-}!Double
        , summaryPCMDoubleRMS   :: {-# UNPACK #-}!Double
        }

    trackIdentifier = const "ZPCMf64b"

    readRaw     = PCM <$> readDouble64be
    readSummary = readSummaryPCMFloat

    prettyRaw         = prettyPacketPCMFloat
    prettySummaryData = prettySummaryPCMFloat

prettyPacketPCMFloat :: PrintfArg a => PCM a -> String
prettyPacketPCMFloat = printf "%.3f" . unPCM

readSummaryPCMFloat :: (I.Nullable s, LL.ListLike s Word8,
                         Functor m, MonadIO m,
                         ZoomReadable (PCM a), ZoomPCMReadable a)
                     => Iteratee s m (SummaryData (PCM a))
readSummaryPCMFloat = do
    [mn,mx,avg,rms] <- replicateM 4 (unPCM <$> readRaw)
    return (pcmMkSummary mn mx avg rms)
{-# SPECIALIZE INLINE readSummaryPCMFloat :: (Functor m, MonadIO m) => Iteratee [Word8] m (SummaryData (PCM Double)) #-}
{-# SPECIALIZE INLINE readSummaryPCMFloat :: (Functor m, MonadIO m) => Iteratee B.ByteString m (SummaryData (PCM Double)) #-}

prettySummaryPCMFloat :: (PrintfArg a, ZoomPCMReadable a)
                      => SummaryData (PCM a) -> String
prettySummaryPCMFloat s = concat
    [ printf "\tmin: %.3f\tmax: %.3f\t" (pcmMin s) (pcmMax s)
    , printf "avg: %.3f\trms: %.3f" (pcmAvg s) (pcmRMS s)
    ]

{-
    typeOfSummaryData = typeOfSummaryPCMDouble

typeOfSummaryPCMDouble :: SummaryData (PCM Double) -> TypeRep
typeOfSummaryPCMDouble _ = mkTyConApp tyCon [d,d,d,d]
    where
        tyCon = mkTyCon3 "zoom-cache" "Data.ZoomCache.Types" "SummaryPCMDouble"
        d = typeOf (undefined :: Double)
-}

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
        , swPCMFloatSum   :: {-# UNPACK #-}!Float
        , swPCMFloatSumSq :: {-# UNPACK #-}!Float
        }
    fromRaw           = fromFloat . unPCM
    fromSummaryData   = fromSummaryPCMFloat

    initSummaryWork   = initSummaryPCMFloat
    toSummaryData     = mkSummaryPCMFloat
    updateSummaryData = updateSummaryPCM
    appendSummaryData = appendSummaryPCMFloat

instance ZoomPCMWritable Float where
    pcmWorkTime = swPCMFloatTime
    pcmWorkMin = swPCMFloatMin
    pcmWorkMax = swPCMFloatMax
    pcmWorkSum = swPCMFloatSum
    pcmWorkSumSq = swPCMFloatSumSq

    pcmMkSummaryWork = SummaryWorkPCMFloat

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
    fromRaw           = fromDouble . unPCM
    fromSummaryData   = fromSummaryPCMDouble

    initSummaryWork   = initSummaryPCMFloat
    toSummaryData     = mkSummaryPCMFloat
    updateSummaryData = updateSummaryPCM
    appendSummaryData = appendSummaryPCMFloat

instance ZoomPCMWritable Double where
    pcmWorkTime = swPCMDoubleTime
    pcmWorkMin = swPCMDoubleMin
    pcmWorkMax = swPCMDoubleMax
    pcmWorkSum = swPCMDoubleSum
    pcmWorkSumSq = swPCMDoubleSumSq

    pcmMkSummaryWork = SummaryWorkPCMDouble

initSummaryPCMFloat :: (Fractional a, ZoomPCMWritable a)
                    => TimeStamp -> SummaryWork (PCM a)
initSummaryPCMFloat entry = pcmMkSummaryWork
    entry
    1000.0 -- floatMax
    (-1000.0) -- negate floatMax
    0.0
    0.0

mkSummaryPCMFloat :: (Floating a, ZoomPCMReadable a, ZoomPCMWritable a)
                  => Double -> SummaryWork (PCM a)
                  -> SummaryData (PCM a)
mkSummaryPCMFloat dur sw =
    pcmMkSummary (pcmWorkMin sw) (pcmWorkMax sw)
                 (pcmWorkSum sw / dur')
                 (sqrt $ pcmWorkSumSq sw / dur')
    where
        !dur' = realToFrac dur

fromSummaryPCMFloat :: SummaryData (PCM Float) -> Builder
fromSummaryPCMFloat s = mconcat $ map fromFloat
    [ pcmMin s , pcmMax s , pcmAvg s , pcmRMS s ]

fromSummaryPCMDouble :: SummaryData (PCM Double) -> Builder
fromSummaryPCMDouble s = mconcat $ map fromDouble
    [ pcmMin s , pcmMax s , pcmAvg s , pcmRMS s ]

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

appendSummaryPCMFloat :: (Ord a, Floating a, ZoomPCMReadable a)
                      => Double -> SummaryData (PCM a)
                      -> Double -> SummaryData (PCM a)
                      -> SummaryData (PCM a)
appendSummaryPCMFloat dur1 s1 dur2 s2 = pcmMkSummary
    (min (pcmMin s1) (pcmMin s2))
    (max (pcmMax s1) (pcmMax s2))
    (((pcmAvg s1 * dur1') + (pcmAvg s2 * dur2')) / durSum)
    (sqrt $ ((pcmRMS s1 * pcmRMS s1 * dur1') +
             (pcmRMS s2 * pcmRMS s2 * dur2')) /
            durSum)
    where
        !dur1' = realToFrac dur1
        !dur2' = realToFrac dur2
        !durSum = realToFrac $ dur1 + dur2

