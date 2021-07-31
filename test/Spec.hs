import           Codec.Picture            (PixelRGBA8 (..))
import           Control.Lens             hiding (elements)
import           Data.Foldable            (fold)
import           Linear.V2
import           Numeric.Natural
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck    as QC

import           Graphics.Heatmap
import           Graphics.Heatmap.Schemes
import           Graphics.Heatmap.Types

instance Arbitrary Depth where arbitrary = Depth <$> arbitrary

propDepthDoesNotDecrease :: Depth -> Depth -> Property
propDepthDoesNotDecrease d1 d2 = counterexample ("got: " <> show dsum) $ dsum >= d1 && dsum >= d2
  where dsum = d1 <> d2

data SrcRange = SrcRange (V2 Double, V2 Double) (V2 Double)
  deriving (Show, Eq)

instance Arbitrary SrcRange where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    xo <- choose (x, x + 100.0)
    yo <- choose (y, y + 100.0)
    xh <- choose (xo, xo + 100.0)
    yh <- choose (yo, yo + 100.0)

    pure $ SrcRange (V2 x y, V2 xh yh) (V2 xo yo)

newtype DstRange = DstRange (V2 Natural, V2 Natural) deriving (Eq, Show)

instance Arbitrary DstRange where
  arbitrary = do
    x <- getNonNegative <$> arbitrary
    y <- getNonNegative <$> arbitrary
    xh <- chooseInt (x, x + 1000)
    yh <- chooseInt (y, y + 1000)

    pure . DstRange $ (V2 x y, V2 xh yh) & each . each %~ toEnum

propTranslatesIntoBounds :: SrcRange -> DstRange -> Property
propTranslatesIntoBounds (SrcRange src p) (DstRange dst) =
  counterexample (fold [
                     "src low:  ", show (fst src), "\n",
                     "src high: ", show (snd src), "\n",
                     "dst low:  ", show (fst dst), "\n",
                     "dst high: ", show (snd dst), "\n",
                     "checked:  ", show p, "\n",
                     "got ", show got]) $
  x `within` (dst ^? _1 . _x, dst ^? _2 . _x) &&
  y `within` (dst ^? _1 . _y, dst ^? _2 . _y)
  where
    got@(V2 x y) = mkTranslator src dst p
    infix 4 >=?, <=?
    _ >=? Nothing = False
    x >=? Just x' = x >= x'
    _ <=? Nothing = False
    x <=? Just x' = x <= x'
    within x (l, h) = x >=? l && x <=? h

newtype AScheme = AScheme (String, ColorScheme) deriving Eq

instance Show AScheme where
  show (AScheme (n,l)) = "AScheme (" <> show n <> ", " <> show (length l) <> ")"

instance Arbitrary AScheme where
  arbitrary = AScheme <$> elements allSchemes

propSchemesColorize :: AScheme -> Depth -> Bool
propSchemesColorize (AScheme (_,sch)) d = seq (schemeColorizer sch d) True

tests :: [TestTree]
tests = [
    testProperty "depth doesn't get smaller" propDepthDoesNotDecrease,
    localOption (QC.QuickCheckTests 5000) $ testProperty "translator is within bounds" propTranslatesIntoBounds,
    localOption (QC.QuickCheckTests 5000) $ testProperty "schemes colorize" propSchemesColorize
    ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
