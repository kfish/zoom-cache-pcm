{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Data.ZoomCache.PCM.Enumeratee
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- ZoomCache PCM enumeratee conversions
----------------------------------------------------------------------

module Data.ZoomCache.PCM.Enumeratee (
      enumPCMDouble
    , enumListPCMDouble

    , enumSummaryPCMDouble
    , wholeTrackSummaryPCMDouble
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Int
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Typeable
import Data.TypeLevel.Num hiding ((==))
import Data.ZoomCache
import Data.ZoomCache.Codec
import Data.ZoomCache.NList

import Data.ZoomCache.PCM.Types
import Data.ZoomCache.PCM.IEEE754()
import Data.ZoomCache.PCM.Int()

----------------------------------------------------------------------

rawToPCMDouble :: ZoomRaw -> [PCM Double]
rawToPCMDouble (ZoomRaw xs) | typeOf xs == typeOf (undefined :: [PCM Double]) =
                                 fromMaybe [] (cast xs :: Maybe [PCM Double])
                            | typeOf xs == typeOf (undefined :: [PCM Float]) =
                                            f (cast xs :: Maybe [PCM Float])
                            | typeOf xs == typeOf (undefined :: [PCM Int]) =
                                            f (cast xs :: Maybe [PCM Int])
                            | typeOf xs == typeOf (undefined :: [PCM Int8]) =
                                            f (cast xs :: Maybe [PCM Int8])
                            | typeOf xs == typeOf (undefined :: [PCM Int16]) =
                                            f (cast xs :: Maybe [PCM Int16])
                            | typeOf xs == typeOf (undefined :: [PCM Int32]) =
                                            f (cast xs :: Maybe [PCM Int32])
                            | typeOf xs == typeOf (undefined :: [PCM Int64]) =
                                            f (cast xs :: Maybe [PCM Int64])
                            | otherwise = []
    where
        f :: Real a => Maybe [PCM a] -> [PCM Double]
        f = maybe [] (map (PCM . realToFrac . unPCM))

rawToListPCMDouble :: ZoomRaw -> [[PCM Double]]
rawToListPCMDouble (ZoomRaw xs) | not (null d) = [d]
                                | typeOf xs == typeOf (undefined :: [NList D1 (PCM Double)]) =
                                                l (cast xs :: Maybe [NList D1 (PCM Double)])
                                | typeOf xs == typeOf (undefined :: [NList D1 (PCM Float)]) =
                                                f (cast xs :: Maybe [NList D1 (PCM Float)])
                                | typeOf xs == typeOf (undefined :: [NList D1 (PCM Int)]) =
                                                f (cast xs :: Maybe [NList D1 (PCM Int)])
                                | typeOf xs == typeOf (undefined :: [NList D1 (PCM Int8)]) =
                                                f (cast xs :: Maybe [NList D1 (PCM Int8)])
                                | typeOf xs == typeOf (undefined :: [NList D1 (PCM Int16)]) =
                                                f (cast xs :: Maybe [NList D1 (PCM Int16)])
                                | typeOf xs == typeOf (undefined :: [NList D1 (PCM Int32)]) =
                                                f (cast xs :: Maybe [NList D1 (PCM Int32)])
                                | typeOf xs == typeOf (undefined :: [NList D1 (PCM Int64)]) =
                                                f (cast xs :: Maybe [NList D1 (PCM Int64)])
                                | otherwise = []
    where
        d = rawToPCMDouble (ZoomRaw xs)
        l :: Maybe [NList D1 a] -> [[a]]
        l = maybe [] (map nListToList)
        f :: (ZoomReadable a) => Maybe [NList D1 a] -> [[PCM Double]]
        f = map (rawToPCMDouble . ZoomRaw) . l

----------------------------------------------------------------------

-- | Coercion of numeric Summary to type Summary Double.
toSummaryPCMDouble :: Typeable a => Summary a -> Maybe (Summary (PCM Double))
toSummaryPCMDouble s | typeOf s == typeOf (undefined :: Summary (PCM Double)) =
                                   id (cast s :: Maybe (Summary (PCM Double)))
                     | typeOf s == typeOf (undefined :: Summary (PCM Float)) =
                               sd <$> (cast s :: Maybe (Summary (PCM Float)))
                     | typeOf s == typeOf (undefined :: Summary (PCM Int)) =
                               sd <$> (cast s :: Maybe (Summary (PCM Int)))
                     | typeOf s == typeOf (undefined :: Summary (PCM Int8)) =
                               sd <$> (cast s :: Maybe (Summary (PCM Int8)))
                     | typeOf s == typeOf (undefined :: Summary (PCM Int16)) =
                               sd <$> (cast s :: Maybe (Summary (PCM Int16)))
                     | typeOf s == typeOf (undefined :: Summary (PCM Int32)) =
                               sd <$> (cast s :: Maybe (Summary (PCM Int32)))
                     | typeOf s == typeOf (undefined :: Summary (PCM Int64)) =
                               sd <$> (cast s :: Maybe (Summary (PCM Int64)))
                     | otherwise = Nothing
    where
        sd :: ZoomPCM a => Summary (PCM a) -> Summary (PCM Double)
        sd s' = s' { summaryData = toSummaryDataPCMDouble (summaryData s') }

toSummaryDataPCMDouble :: ZoomPCM a => SummaryData (PCM a) -> SummaryData (PCM Double)
toSummaryDataPCMDouble s = pcmMkSummary
    (realToFrac . pcmMin $ s)
    (realToFrac . pcmMax $ s)
    (pcmAvg s)
    (pcmRMS s)

----------------------------------------------------------------------

enumPCMDouble :: (Functor m, MonadIO m)
              => I.Enumeratee [Stream] [(TimeStamp, PCM Double)] m a
enumPCMDouble = I.joinI . enumPackets . I.mapChunks (concatMap f)
    where
        f :: Packet -> [(TimeStamp, PCM Double)]
        f Packet{..} = zip packetTimeStamps (rawToPCMDouble packetData)

enumListPCMDouble :: (Functor m, MonadIO m)
                  => I.Enumeratee [Stream] [(TimeStamp, [PCM Double])] m a
enumListPCMDouble = I.joinI . enumPackets . I.mapChunks (concatMap f)
    where
        f :: Packet -> [(TimeStamp, [PCM Double])]
        f Packet{..} = zip packetTimeStamps (rawToListPCMDouble packetData)

----------------------------------------------------------------------

-- | Read the summary of an entire track.
wholeTrackSummaryPCMDouble :: (Functor m, MonadIO m)
                           => [IdentifyCodec]
                           -> TrackNo
                           -> I.Iteratee ByteString m (Summary (PCM Double))
wholeTrackSummaryPCMDouble identifiers trackNo = I.joinI $ enumCacheFile identifiers .
    I.joinI . filterTracks [trackNo] .  I.joinI . e $ I.last
    where
        e = I.joinI . enumSummaries . I.mapChunks (catMaybes . map toSD)
        toSD :: ZoomSummary -> Maybe (Summary (PCM Double))
        toSD (ZoomSummary s) = toSummaryPCMDouble s

enumSummaryPCMDouble :: (Functor m, MonadIO m)
                     => Int
                     -> I.Enumeratee [Stream] [Summary (PCM Double)] m a
enumSummaryPCMDouble level =
    I.joinI . enumSummaryLevel level .
    I.mapChunks (catMaybes . map toSD)
    where
        toSD :: ZoomSummary -> Maybe (Summary (PCM Double))
        toSD (ZoomSummary s) = toSummaryPCMDouble s
