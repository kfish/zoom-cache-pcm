{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM (
    -- * PCM Type
      PCM(..)

    -- * Codec identifiers
    , pcmIdentifiers
) where

import Data.ByteString (ByteString)
import Data.Int
import Data.ZoomCache.Codec (Codec, identifyCodec)

import Data.ZoomCache.PCM.Types

import Data.ZoomCache.PCM.IEEE754()
import Data.ZoomCache.PCM.Int()

----------------------------------------------------------------------

pcmIdentifiers :: [ByteString -> Maybe Codec]
pcmIdentifiers =
    [ identifyCodec (undefined :: PCM Float)
    , identifyCodec (undefined :: PCM Double)
    , identifyCodec (undefined :: PCM Int)
    , identifyCodec (undefined :: PCM Int16)
    , identifyCodec (undefined :: PCM Int32)
    , identifyCodec (undefined :: PCM Int64)
    ]
