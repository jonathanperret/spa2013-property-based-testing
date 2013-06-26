module Spec where
import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Data.Ratio
import Control.Applicative

data Unit = Meter | Second | Unitless | Unit :*: Unit
  deriving (Show, Eq)

data Measure = Measure { value :: Rational, unit :: Unit } | InvalidMeasure
  deriving (Show, Eq)

instance Arbitrary Unit where
  arbitrary = elements [ Meter, Second, Unitless ]

instance Arbitrary Measure where
  arbitrary = Measure <$> arbitrary <*> arbitrary

instance Num Measure where
  Measure x u1 + Measure y u2
    | u1 == u2 = Measure (x + y) u1
    | otherwise = InvalidMeasure

  negate (Measure x u) = Measure (-x) u

  Measure x u1 * Measure y u2 =
    Measure (x*y) (combine u1 u2)
    where
      combine Unitless u2 = u2
      combine u1 Unitless = u1
      combine u1 u2 = (u1 :*: u2)

main = hspec $ do
  describe "Arithmetic on measures" $ do
    it "adds measures expressed in the same unit" $ property $
      \(x, y, u)->
        Measure x u + Measure y u == Measure (x + y) u
    it "does adds measures expressed in different units" $ property $
      \(m1, m2)->
        unit m1 /= unit m2 ==>
          m1 + m2 == InvalidMeasure
    it "subtracts measures expressed in the same unit" $ property $
      \(x, y, u)->
        Measure x u - Measure y u == Measure (x - y) u
    it "multiplies a measure by a unitless measure" $ property $
      \(x, y, u)->
        Measure x u * Measure y Unitless == Measure (x*y) u
        && Measure x Unitless * Measure y u == Measure (x*y) u
