{-|
Module      : Graphics.Heatmap.SchemeGen
Description : Generate heatmaps from PNGs using TemplateHaskell.
Copyright   : (c) Dustin Sallings, 2021
License     : BSD3
Maintainer  : dustin@spy.net
Stability   : experimental

Generate heatmaps from PNGs using TemplateHaskell.
-}

module Graphics.Heatmap.SchemeGen (genScheme, genSchemes) where

import           Codec.Picture          (Image (..), PixelRGBA8 (..), convertRGBA8, imageHeight, pixelAt, readImage)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Maybe             (mapMaybe)
import           Language.Haskell.TH
import           System.Directory       (listDirectory)
import           System.FilePath.Posix  (splitExtension, (</>))

-- | Generate a color scheme from a PNG on the filesystem.
genScheme :: String -> FilePath -> Q [Dec]
genScheme name fp = runIO $ do
  i <- fmap convertRGBA8 <$> readImage fp
  img <- case i of
           Right img -> pure img
           Left x    -> fail ("error loading image " <> fp <> ": " <> x)
  let pixies = map (pixelAt img 0) [0.. imageHeight img - 1]
  pure [SigD (mkName name) (ConT (mkName "ColorScheme")),
        ValD (VarP (mkName name))
        (NormalB (ListE [AppE (AppE (AppE (AppE (ConE (mkName "PixelRGBA8"))
                                    (LitE (IntegerL (toInteger r))))
                                  (LitE (IntegerL (toInteger g))))
                              (LitE (IntegerL (toInteger b))))
                          (LitE (IntegerL (toInteger a)))
                        | PixelRGBA8 r g b a <- pixies])) []]

findSchemes :: FilePath -> IO [(String, FilePath)]
findSchemes fp = mapMaybe identify <$> listDirectory fp

    where identify p = case splitExtension p of
                         (b, ".png") -> Just (b, fp </> p)
                         _           -> Nothing

cmapM :: (Monoid b, Applicative f) => (a -> f b) -> [a] -> f b
cmapM f xs = mconcat <$> traverse f xs

-- | Generate color schemes for all PNGs found within a given directory.
--
-- Each scheme is named after the basename of the png that's found.
-- Additionally, a list of all the generated schemes named
-- 'allSchemes' will be generated.
genSchemes :: FilePath -> Q [Dec]
genSchemes fp = do
  schemeFiles <- liftIO (findSchemes fp)
  defs <- cmapM (uncurry genScheme) schemeFiles
  let allName = mkName "allSchemes"
      ss = [SigD allName (AppT ListT (AppT (AppT (TupleT 2) (ConT (mkName "String")))
                                      (ConT (mkName "ColorScheme")))),
            ValD (VarP allName) (NormalB (ListE [TupE [
                                                    Just (LitE (StringL s)), Just (VarE (mkName s))]
                                                    | (s,_) <- schemeFiles])) []]
  pure (defs <> ss)
