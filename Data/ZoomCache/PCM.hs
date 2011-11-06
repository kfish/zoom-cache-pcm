{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM (
    -- * PCM Type
      PCM(..)

    -- * Track type identifiers
    , pcmIdentifiers
) where

import Data.ZoomCache.PCM.Types

import Data.ZoomCache.PCM.Double()
import Data.ZoomCache.PCM.Int()

import Data.ByteString (ByteString)
import Data.ZoomCache.Codec (TrackType, identifyTrackType)

pcmIdentifiers :: [ByteString -> Maybe TrackType]
pcmIdentifiers =
    [ identifyTrackType (undefined :: PCM Float)
    , identifyTrackType (undefined :: PCM Double)
    , identifyTrackType (undefined :: PCM Int)
    ]
