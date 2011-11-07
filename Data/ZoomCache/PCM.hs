{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM (
    -- * PCM Type
      PCM(..)

    -- * Codec identifiers
    , pcmIdentifiers
) where

import Data.ZoomCache.PCM.Types

import Data.ZoomCache.PCM.IEEE754()
import Data.ZoomCache.PCM.Int()

import Data.ByteString (ByteString)
import Data.ZoomCache.Codec (Codec, identifyCodec)

pcmIdentifiers :: [ByteString -> Maybe Codec]
pcmIdentifiers =
    [ identifyCodec (undefined :: PCM Float)
    , identifyCodec (undefined :: PCM Double)
    , identifyCodec (undefined :: PCM Int)
    ]
