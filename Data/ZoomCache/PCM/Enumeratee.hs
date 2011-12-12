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
) where

import Control.Monad.Trans (MonadIO)
import Data.Int
import qualified Data.Iteratee as I
import Data.Maybe
import Data.Typeable
import Data.Word
import Data.ZoomCache

import Data.ZoomCache.PCM.Types

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
                            | typeOf xs == typeOf (undefined :: [PCM Integer]) =
                                            f (cast xs :: Maybe [PCM Integer])
                            | typeOf xs == typeOf (undefined :: [PCM Word]) =
                                            f (cast xs :: Maybe [PCM Word])
                            | typeOf xs == typeOf (undefined :: [PCM Word8]) =
                                            f (cast xs :: Maybe [PCM Word8])
                            | typeOf xs == typeOf (undefined :: [PCM Word16]) =
                                            f (cast xs :: Maybe [PCM Word16])
                            | typeOf xs == typeOf (undefined :: [PCM Word32]) =
                                            f (cast xs :: Maybe [PCM Word32])
                            | typeOf xs == typeOf (undefined :: [PCM Word64]) =
                                            f (cast xs :: Maybe [PCM Word64])
                            | otherwise = []
    where
        f :: Real a => Maybe [PCM a] -> [PCM Double]
        f = maybe [] (map (PCM . realToFrac . unPCM))

----------------------------------------------------------------------

enumPCMDouble :: (Functor m, MonadIO m)
              => I.Enumeratee [Stream] [(TimeStamp, PCM Double)] m a
enumPCMDouble = I.joinI . enumPackets . I.mapChunks (concatMap f)
    where
        f :: Packet -> [(TimeStamp, PCM Double)]
        f Packet{..} = zip packetTimeStamps (rawToPCMDouble packetData)

