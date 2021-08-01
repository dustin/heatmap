import           Codec.Picture            (PixelRGBA8 (..))
import           Control.Applicative      (liftA2)
import           Control.Lens             hiding (elements)
import           Control.Monad            (void)
import           Data.Foldable            (fold)
import           Hedgehog
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range
import           Linear.V2
import           Numeric.Natural
import           Test.Tasty
import           Test.Tasty.Hedgehog

import           Graphics.Heatmap
import           Graphics.Heatmap.Schemes
import           Graphics.Heatmap.Types

genDepth :: Gen Depth
genDepth = Gen.enumBounded

propDepthDoesNotDecrease :: Property
propDepthDoesNotDecrease = property $ do
  (d1,d2) <- forAll $ liftA2 (,) genDepth genDepth
  let dsum = d1 <> d2
  annotateShow dsum
  assert $ dsum >= d1 && dsum >= d2

data SrcRange = SrcRange (V2 Double, V2 Double) (V2 Double)
  deriving (Show, Eq)

genSrcRange :: Gen SrcRange
genSrcRange = do
    x  <- Gen.double $ Range.linearFrac (-10000) 10000
    y  <- Gen.double $ Range.linearFrac (-10000) 10000
    xo <- Gen.double $ Range.linearFrac x (x + 100)
    yo <- Gen.double $ Range.linearFrac y (y + 100)
    xh <- Gen.filter (> x) . Gen.double $ Range.linearFrac xo (xo + 100)
    yh <- Gen.filter (> y) . Gen.double $ Range.linearFrac yo (yo + 100)

    pure $ SrcRange (V2 x y, V2 xh yh) (V2 xo yo)

newtype DstRange = DstRange (V2 Natural, V2 Natural) deriving (Eq, Show)

genDstRange :: Gen DstRange
genDstRange = do
    x <- Gen.integral $ Range.linear 0 1000
    y <- Gen.integral $ Range.linear 0 1000
    xh <- Gen.integral $ Range.linear (x+1) (x + 1000)
    yh <- Gen.integral $ Range.linear (y+1) (y + 1000)

    pure . DstRange $ (V2 x y, V2 xh yh) & each . each %~ toEnum

propTranslatesIntoBounds :: Property
propTranslatesIntoBounds = property $ do
  (SrcRange src p, DstRange dst) <- forAll $ liftA2 (,) genSrcRange genDstRange
  let got@(V2 x y) = mkTranslator src dst p
  annotate (fold [
               "src low:  ", show (fst src), "\n",
               "src high: ", show (snd src), "\n",
               "dst low:  ", show (fst dst), "\n",
               "dst high: ", show (snd dst), "\n",
               "checked:  ", show p, "\n",
               "got ", show got])
  assert $ x `within` (dst ^? _1 . _x, dst ^? _2 . _x) && y `within` (dst ^? _1 . _y, dst ^? _2 . _y)

  where
    infix 4 >=?, <=?
    _ >=? Nothing = False
    x >=? Just x' = x >= x'
    _ <=? Nothing = False
    x <=? Just x' = x <= x'
    within x (l, h) = x >=? l && x <=? h

propSchemesColorize :: Property
propSchemesColorize = property $ do
  ((_,sch), d) <- forAll $ liftA2 (,) (Gen.element allSchemes) Gen.enumBounded
  void . eval $ schemeColorizer sch d

tests :: [TestTree]
tests = [
  testProperty "depth doesn't get smaller" propDepthDoesNotDecrease,
  localOption (HedgehogTestLimit (Just 5000)) $ testProperty "translator is within bounds" propTranslatesIntoBounds,
  localOption (HedgehogTestLimit (Just 5000)) $ testProperty "schemes colorize" propSchemesColorize
    ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
