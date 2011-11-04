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

import Data.ByteString.Lazy (ByteString)
import Data.ZoomCache (TrackType, ttMapping)

pcmMappings :: [ByteString -> Maybe TrackType]
pcmMappings =
    [ ttMapping (undefined :: PCM Double)
    , ttMapping (undefined :: PCM Int)
    ]
