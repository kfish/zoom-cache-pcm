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

Default codec implementation for PCM Audio of type Double. This module
implements the interfaces documented in "Data.ZoomCache.Codec".

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

----------------------------------------------------------------------
-- Read

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

    initSummaryWork   = initSummaryPCMDouble
    toSummaryData     = mkSummaryPCMDouble
    updateSummaryData = updateSummaryPCMDouble
    appendSummaryData = appendSummaryPCMDouble

initSummaryPCMDouble :: TimeStamp -> SummaryWork (PCM Double)
initSummaryPCMDouble entry = SummaryWorkPCMDouble
    { swPCMDoubleTime = entry
    , swPCMDoubleMin = 1000.0 -- floatMax
    , swPCMDoubleMax = -1000.0 -- negate floatMax
    , swPCMDoubleSum = 0.0
    , swPCMDoubleSumSq = 0.0
    }

mkSummaryPCMDouble :: Double -> SummaryWork (PCM Double)
                   -> SummaryData (PCM Double)
mkSummaryPCMDouble dur SummaryWorkPCMDouble{..} = SummaryPCMDouble
    { summaryPCMDoubleMin = swPCMDoubleMin
    , summaryPCMDoubleMax = swPCMDoubleMax
    , summaryPCMDoubleAvg = swPCMDoubleSum / dur
    , summaryPCMDoubleRMS = sqrt $ swPCMDoubleSumSq / dur
    }

fromSummaryPCMDouble :: SummaryData (PCM Double) -> Builder
fromSummaryPCMDouble SummaryPCMDouble{..} = mconcat $ map fromDouble
    [ summaryPCMDoubleMin
    , summaryPCMDoubleMax
    , summaryPCMDoubleAvg
    , summaryPCMDoubleRMS
    ]

updateSummaryPCMDouble :: TimeStamp -> PCM Double
                       -> SummaryWork (PCM Double)
                       -> SummaryWork (PCM Double)
updateSummaryPCMDouble t (PCM d) SummaryWorkPCMDouble{..} = SummaryWorkPCMDouble
    { swPCMDoubleTime = t
    , swPCMDoubleMin = min swPCMDoubleMin d
    , swPCMDoubleMax = max swPCMDoubleMax d
    , swPCMDoubleSum = swPCMDoubleSum + (d * dur)
    , swPCMDoubleSumSq = swPCMDoubleSumSq + (d*d * dur)
    }
    where
        !dur = fromIntegral $ (unTS t) - (unTS swPCMDoubleTime)

appendSummaryPCMDouble :: Double -> SummaryData (PCM Double)
                       -> Double -> SummaryData (PCM Double)
                       -> SummaryData (PCM Double)
appendSummaryPCMDouble dur1 s1 dur2 s2 = SummaryPCMDouble
    { summaryPCMDoubleMin = min (summaryPCMDoubleMin s1) (summaryPCMDoubleMin s2)
    , summaryPCMDoubleMax = max (summaryPCMDoubleMax s1) (summaryPCMDoubleMax s2)
    , summaryPCMDoubleAvg = ((summaryPCMDoubleAvg s1 * dur1) +
                             (summaryPCMDoubleAvg s2 * dur2)) /
                            durSum
    , summaryPCMDoubleRMS = sqrt $ ((summaryPCMDoubleRMS s1 * summaryPCMDoubleRMS s1 * dur1) +
                                 (summaryPCMDoubleRMS s2 * summaryPCMDoubleRMS s2 * dur2)) /
                                durSum
    }
    where
        !durSum = dur1 + dur2

