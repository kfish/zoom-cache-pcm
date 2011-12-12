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
) where

import Control.Monad.Trans (MonadIO)
import Data.Int
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Typeable
import Data.TypeLevel.Num hiding ((==))
import Data.ZoomCache
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

