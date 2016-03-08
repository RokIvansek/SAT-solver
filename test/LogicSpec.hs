module LogicSpec where

import Test.Hspec
import Data
import Logic

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Simplify" $ do
    it "should remove double negation" $ do
      simplify (Not (Not (Lit "a")))`shouldBe` (Lit "a")
