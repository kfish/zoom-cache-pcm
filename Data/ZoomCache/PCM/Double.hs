{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
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
   | Entry (double)                                                | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Exit (double)                                                 | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Min (double)                                                  | 52-55
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 56-59
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Max (double)                                                  | 60-63
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 64-67
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Avg (double)                                                  | 68-71
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 72-75
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | RMS (double)                                                  | 76-79
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                                                               | 80-83
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
import Data.Iteratee (Iteratee)
import Data.Monoid
import Data.Word
import Text.Printf

import Data.ZoomCache.Codec
import Data.ZoomCache.PCM.Types
-- import Numeric.FloatMinMax

----------------------------------------------------------------------
-- Read

instance ZoomReadable (PCM Double) where
    data SummaryData (PCM Double) = SummaryPCMDouble
        { summaryPCMDoubleEntry :: {-# UNPACK #-}!Double
        , summaryPCMDoubleExit  :: {-# UNPACK #-}!Double
        , summaryPCMDoubleMin   :: {-# UNPACK #-}!Double
        , summaryPCMDoubleMax   :: {-# UNPACK #-}!Double
        , summaryPCMDoubleAvg   :: {-# UNPACK #-}!Double
        , summaryPCMDoubleRMS   :: {-# UNPACK #-}!Double
        }

    readRaw     = PCM <$> readDouble64be
    readSummary = readSummaryPCMDouble

    prettyRaw         = prettyPacketPCMDouble
    prettySummaryData = prettySummaryPCMDouble

prettyPacketPCMDouble :: PCM Double -> String
prettyPacketPCMDouble = printf "%.3f" . unPCM

readSummaryPCMDouble :: (Functor m, MonadIO m)
                     => Iteratee [Word8] m (SummaryData (PCM Double))
readSummaryPCMDouble = do
    [en,ex,mn,mx,avg,rms] <- replicateM 6 readDouble64be
    return (SummaryPCMDouble en ex mn mx avg rms)

prettySummaryPCMDouble :: SummaryData (PCM Double) -> String
prettySummaryPCMDouble SummaryPCMDouble{..} = concat
    [ printf "\tentry: %.3f\texit: %.3f\tmin: %.3f\tmax: %.3f\t"
          summaryPCMDoubleEntry summaryPCMDoubleExit summaryPCMDoubleMin summaryPCMDoubleMax
    , printf "avg: %.3f\trms: %.3f" summaryPCMDoubleAvg summaryPCMDoubleRMS
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
        , swPCMDoubleEntry :: {-# UNPACK #-}!Double
        , swPCMDoubleExit  :: {-# UNPACK #-}!Double
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
    , swPCMDoubleEntry = 0.0
    , swPCMDoubleExit = 0.0
    , swPCMDoubleMin = 1000.0 -- floatMax
    , swPCMDoubleMax = -1000.0 -- negate floatMax
    , swPCMDoubleSum = 0.0
    , swPCMDoubleSumSq = 0.0
    }

mkSummaryPCMDouble :: Double -> SummaryWork (PCM Double)
                   -> SummaryData (PCM Double)
mkSummaryPCMDouble dur SummaryWorkPCMDouble{..} = SummaryPCMDouble
    { summaryPCMDoubleEntry = swPCMDoubleEntry
    , summaryPCMDoubleExit = swPCMDoubleExit
    , summaryPCMDoubleMin = swPCMDoubleMin
    , summaryPCMDoubleMax = swPCMDoubleMax
    , summaryPCMDoubleAvg = swPCMDoubleSum / dur
    , summaryPCMDoubleRMS = sqrt $ swPCMDoubleSumSq / dur
    }

fromSummaryPCMDouble :: SummaryData (PCM Double) -> Builder
fromSummaryPCMDouble SummaryPCMDouble{..} = mconcat $ map fromDouble
    [ summaryPCMDoubleEntry
    , summaryPCMDoubleExit
    , summaryPCMDoubleMin
    , summaryPCMDoubleMax
    , summaryPCMDoubleAvg
    , summaryPCMDoubleRMS
    ]

updateSummaryPCMDouble :: Int -> TimeStamp -> PCM Double
                       -> SummaryWork (PCM Double)
                       -> SummaryWork (PCM Double)
updateSummaryPCMDouble count t (PCM d) SummaryWorkPCMDouble{..} = SummaryWorkPCMDouble
    { swPCMDoubleTime = t
    , swPCMDoubleEntry = if count == 0 then d else swPCMDoubleEntry
    , swPCMDoubleExit = d
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
    { summaryPCMDoubleEntry = summaryPCMDoubleEntry s1
    , summaryPCMDoubleExit = summaryPCMDoubleExit s2
    , summaryPCMDoubleMin = min (summaryPCMDoubleMin s1) (summaryPCMDoubleMin s2)
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

