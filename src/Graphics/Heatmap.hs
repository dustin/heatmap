{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Module      : Graphics.Heatmap
Description : Heatmap generation utilities.
Copyright   : (c) Dustin Sallings, 2021
License     : BSD3
Maintainer  : dustin@spy.net
Stability   : experimental

Tools for generating heatmap images.
-}

module Graphics.Heatmap (Point, mkDot, genHeatmap, mkTranslator) where

import           Codec.Picture          (Image (..), PixelRGBA8 (..), generateImage)
import           Control.Lens
import qualified Data.Map.Strict        as Map
import           Graphics.Heatmap.Types (Colorizer, Depth (..))
import           Linear.V2              (V2 (..))
import           Linear.Vector          (zero)
import           Numeric.Natural

-- | A Point as used throughout this heatmap codebase is currently
-- limited to two dimensional vectors of natural numbers.
type Point = V2 Natural

class Projector a b | a -> b where
  project :: a -> b -> [a]

instance (Enum a, Integral a) => Projector (V2 a) a where
  project o size = [ V2 x y + o | x <- [ 0 .. size ], y <- [ 0 .. size ]]

-- mkDot can be more generic, but we don't have a great use case for
-- things other than V2 right now.
--
-- mkDot :: (Each a a Int Int, Projector a Int, Num a) =>  a -> Int -> [(a, Depth)]

-- | Make a dot of a given size indicating presence that should begin
-- heating the map.
mkDot :: Point -> Natural -> [(Point, Depth)]
mkDot o size = [ d | p2 <- project zero size,
                 d <- depthAt (o + p2) (distance p2) ]
  where
    edge   = o & each .~ size
    middle = distance edge / 2
    distance = sqrt . sumOf (each . to f)
      where f off = (fromIntegral off - (fromIntegral size / 2)) ^^ 2
    depthAt p d = [(p, maxBound - round (200 * d/middle + 50)) | d < middle]

-- | Generate a heatmap image using a Colorizer and a collection of points and depths.
genHeatmap :: (Natural, Natural) -> Colorizer -> [(Point, Depth)] -> Image PixelRGBA8
genHeatmap (w,h) colorizer points = generateImage pf (fromIntegral w) (fromIntegral h)
  where
    m = Map.fromListWith (<>) points
    pf x y = colorizer $ Map.findWithDefault minBound (V2 (fromIntegral x) (fromIntegral y)) m

-- | Make a scale translator that can translate points in an
-- arbitrarily bounded two dimensional vector to our 2D vector with
-- Natural bounds we use when applying points.
mkTranslator :: RealFrac a => (V2 a, V2 a) -> (V2 Natural, V2 Natural) -> (V2 a -> V2 Natural)
mkTranslator (V2 inminx inminy, V2 inmaxx inmaxy) (V2 outminx outminy, V2 outmaxx outmaxy) = t
  where
    t (V2 x y) = V2 (trans x inminx inmaxx outminx outmaxx) (trans y inminy inmaxy outminy outmaxy)
    trans v il ih ol oh = ol + round ((v - il) * fromIntegral (oh - ol) / (ih - il))
