{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Graphics.Heatmap.Types where

import           Codec.Picture (PixelRGB8 (..), PixelRGBA8 (..))
import qualified Data.Vector   as V
import           Data.Word     (Word8)

type Colorizer = Depth -> PixelRGBA8

type ColorScheme = [PixelRGBA8]

newtype Depth = Depth Word8 deriving (Eq, Show, Num, Ord, Enum, Real, Integral, Bounded)

instance Semigroup Depth where
  Depth a <> Depth b = Depth x
    where
      x = if fromEnum a + fromEnum b > fromEnum (maxBound @ Word8) then maxBound else a + b

instance Monoid Depth where
  mempty = 0

-- Simple colorizer that uses a constant color and applies different alpha values.
alphaColorizer :: PixelRGB8 -> Colorizer
alphaColorizer (PixelRGB8 r g b) (Depth a) = PixelRGBA8 r g b a

-- Colorizer that applies a scheme.
schemeColorizer :: ColorScheme -> Colorizer
schemeColorizer = f . V.fromList
  where f v (Depth d) = PixelRGBA8 r g b d
          where
            percent = fromIntegral d / 255.0
            offset = fromIntegral (V.length v) - 1 * 1.0 - percent
            (PixelRGBA8 r g b _) = v V.! round offset
