{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM (
    -- * PCM Type
      PCM(..)

    -- * Track mappings
    , pcmMappings
) where

import Data.ZoomCache.PCM.Types

import Data.ZoomCache.PCM.Double()
import Data.ZoomCache.PCM.Int()

import Data.ByteString (ByteString)
import Data.ZoomCache.Codec (TrackType, identifyTrackType)

pcmMappings :: [ByteString -> Maybe TrackType]
pcmMappings =
    [ identifyTrackType (undefined :: PCM Double)
    , identifyTrackType (undefined :: PCM Int)
    ]
