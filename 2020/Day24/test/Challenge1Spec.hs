{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Challenge1 (traversePath, Coordinate, Path)

main :: IO ()
main = hspec $ do
  describe "traversePath" $ do
    -- Test for an empty path
    it "should handle an empty path from (2, 2)" $ do
      traversePath (2, 2) [] `shouldBe` [(2, 2)]

    -- Simple one-step movements
    it "should handle a single 'e' step from (2, 2)" $ do
      traversePath (2, 2) ["e"] `shouldBe` [(3, 2), (2, 2)]

    it "should handle a single 'w' step from (2, 2)" $ do
      traversePath (2, 2) ["w"] `shouldBe` [(1, 2), (2, 2)]

    it "should handle a single 'ne' step from even row (2, 2)" $ do
      traversePath (2, 2) ["ne"] `shouldBe` [(2, 1), (2, 2)]

    it "should handle a single 'ne' step from odd row (2, 3)" $ do
      traversePath (2, 3) ["ne"] `shouldBe` [(3, 2), (2, 3)]

    it "should handle a single 'nw' step from even row (2, 2)" $ do
      traversePath (2, 2) ["nw"] `shouldBe` [(1, 1), (2, 2)]

    it "should handle a single 'nw' step from odd row (2, 3)" $ do
      traversePath (2, 3) ["nw"] `shouldBe` [(2, 2), (2, 3)]

    it "should handle a single 'se' step from even row (2, 2)" $ do
      traversePath (2, 2) ["se"] `shouldBe` [(2, 3), (2, 2)]

    it "should handle a single 'se' step from odd row (2, 3)" $ do
      traversePath (2, 3) ["se"] `shouldBe` [(3, 4), (2, 3)]

    it "should handle a single 'sw' step from even row (2, 2)" $ do
      traversePath (2, 2) ["sw"] `shouldBe` [(1, 3), (2, 2)]

    it "should handle a single 'sw' step from odd row (2, 3)" $ do
      traversePath (2, 3) ["sw"] `shouldBe` [(2, 4), (2, 3)]

    -- Complex paths
    it "should handle a complex path from (2, 2)" $ do
      traversePath (2, 2) ["e", "ne", "w", "sw", "se"] `shouldBe`
        [(2, 3), (2, 2), (2, 1), (3, 1), (3, 2), (2, 2)]

    -- Looping path from a reference point
    it "should handle a path starting from (2, 2) and moving in a loop" $ do
      traversePath (2, 2) ["e", "se", "sw", "w", "nw", "ne"] `shouldBe`
        [(2, 2), (1, 3), (2, 4), (3, 4), (3, 3), (3, 2), (2, 2)]

    -- A more complex path
    it "should handle a complex path starting from (2, 3)" $ do
      traversePath (2, 3) ["e", "ne", "e", "se", "sw", "w"] `shouldBe`
        [(4,4), (5, 4), (5,3), (5,2), (4, 2), (3, 3), (2, 3)]

    it "starting from (0, 0) do w"$ do
      traversePath (0, 0) ["w"] `shouldBe`
        [(-1, 0), (0, 0)]

    it "starting from (0, 0) do e"$ do
      traversePath (0, 0) ["e"] `shouldBe`
        [(1, 0), (0, 0)]