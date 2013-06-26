module Spec where
import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Data.Ratio

data Unit = Meter | Second
  deriving (Show, Eq)

data Measure = Measure Rational Unit
  deriving (Show, Eq)

instance Num Measure where
   (Measure x Meter) + (Measure y Meter) = (Measure (x + y) Meter)

main = hspec $ do
  describe "Arithmetic on measures" $ do
    it "adds measures expressed in the same unit" $ property $
      \(x, y)->
        (Measure x Meter) + (Measure y Meter) == (Measure (x + y) Meter)
