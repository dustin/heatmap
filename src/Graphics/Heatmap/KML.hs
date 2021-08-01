{-|
Module      : Graphics.Heatmap.KML
Description : Heatmap KML Utilities.
Copyright   : (c) Dustin Sallings, 2021
License     : BSD3
Maintainer  : dustin@spy.net
Stability   : experimental

Heatmap KML utilities.
-}

module Graphics.Heatmap.KML (GeoBounds(..), kmlStr, mkKMZ) where

import           Codec.Archive.Zip
import           Codec.Picture         (Image (..), PixelRGBA8 (..), encodePng)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import           Numeric               (showFloat)
import           Text.XML.Light

-- | Boundaries for displaying overlays.
data GeoBounds = GeoBounds {
  north, south, east, west :: !Double
  } deriving Show

-- | Produce a KML document overlaying the document at the given URL across the given geo boundaries.
kmlStr :: String -> GeoBounds -> String
kmlStr iconPath (GeoBounds n s e w) = showTopElement kml
  where
    elc nm atts stuff = Element blank_name{qName= nm} atts stuff Nothing
    elr nm atts stuff = elc nm atts (Elem <$> stuff)
    elt nm stuff = elc nm [] [t stuff]
    at k v = Attr blank_name{qName=k} v
    t v = Text blank_cdata{cdData=v}

    kml = elr "kml" [at "xmlns" "http://www.opengis.net/kml/2.2"] [folder]
    folder = elr "Folder" [] [groundo]
    groundo = elr "GroundOverlay" [] [icon, box]
    icon = elr "Icon" [] [elt "href" (iconPath)]
    box = elr "LatLonBox" [] [ll "north" n, ll "south" s, ll "east" e, ll "west" w,
                              elt "rotation" "0"]
    ll nm v = elt nm (showFloat v "")

-- | Generate and store a KMZ file with the given image ampped into the given bounds.
mkKMZ :: Image PixelRGBA8 -> GeoBounds -> FilePath -> IO ()
mkKMZ img bounds fp = createArchive fp $ do
  addEntry Store (BL.toStrict $ encodePng img) =<< mkEntrySelector "heatmap.png"
  addEntry Deflate (BC.pack $ kmlStr "heatmap.png" bounds) =<< mkEntrySelector "doc.kml"
