{-# LANGUAGE TemplateHaskell #-}

module Graphics.Heatmap.Schemes where

import           Codec.Picture              (PixelRGBA8 (..))
import           Graphics.Heatmap.SchemeGen (genSchemes)
import           Graphics.Heatmap.Types     (ColorScheme)

genSchemes "src/Graphics/Heatmap/Schemes/"
