{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

module Data.ZoomCache.PCM.Types (
    -- * PCM Type
      PCM(..)
) where

import Data.Typeable

data PCM a = PCM { unPCM :: !a }
    deriving (Typeable)
