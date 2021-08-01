{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Graphics.Heatmap.Schemes
Description : Color schemes for heatmaps.
Copyright   : (c) Dustin Sallings, 2021
License     : BSD3
Maintainer  : dustin@spy.net
Stability   : experimental

Color schemes for heatmaps.

These are all generated from a set of PNGs that might look pleasant on
your heatmap.
-}

module Graphics.Heatmap.Schemes where

import           Codec.Picture              (PixelRGBA8 (..))
import           Graphics.Heatmap.SchemeGen (genSchemes)
import           Graphics.Heatmap.Types     (ColorScheme)

genSchemes "src/Graphics/Heatmap/Schemes/"
