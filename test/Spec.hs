module Main (main) where

import Test.Hspec

import RiskScore (riskScore)
import RiskTypes (RiskLevel(..), parseRiskLevel)

main :: IO ()
main = hspec $ do
  describe "riskScore" $ do
    it "scores low risk" $ riskScore Low `shouldBe` 25
    it "scores medium risk" $ riskScore Medium `shouldBe` 50
    it "scores high risk" $ riskScore High `shouldBe` 75
    it "scores critical risk" $ riskScore Critical `shouldBe` 95

  describe "parseRiskLevel" $ do
    it "accepts case-insensitive values" $
      parseRiskLevel "HiGh" `shouldBe` Right High
    it "rejects invalid values" $
      parseRiskLevel "urgent" `shouldSatisfy` either (const True) (const False)
