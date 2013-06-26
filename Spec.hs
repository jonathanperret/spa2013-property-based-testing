module Spec where
import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Data.Ratio

data Unit = Meter | Second
  deriving (Show, Eq)

data Measure = Measure Rational Unit | InvalidMeasure
  deriving (Show, Eq)

instance Arbitrary Unit where
  arbitrary = elements [ Meter, Second ]

instance Num Measure where
  (Measure x u1) + (Measure y u2)
    | u1 == u2 = (Measure (x + y) u1)
    | otherwise = InvalidMeasure

main = hspec $ do
  describe "Arithmetic on measures" $ do
    it "adds measures expressed in the same unit" $ property $
      \(x, y, u)->
        (Measure x u) + (Measure y u) == (Measure (x + y) u)
    it "does adds measures expressed in different units" $ property $
      \(x, y, u, u2)->
        u == u2 || (Measure x u) + (Measure y u2) == InvalidMeasure
