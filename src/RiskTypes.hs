module RiskTypes
  ( RiskLevel(..)
  , parseRiskLevel
  , renderRiskLevel
  ) where

import Data.Char (toLower)

data RiskLevel = Low | Medium | High | Critical
  deriving (Eq, Show)

parseRiskLevel :: String -> Either String RiskLevel
parseRiskLevel raw =
  case map toLower raw of
    "low" -> Right Low
    "medium" -> Right Medium
    "high" -> Right High
    "critical" -> Right Critical
    _ -> Left "Risk level must be one of: low, medium, high, critical."

renderRiskLevel :: RiskLevel -> String
renderRiskLevel level =
  case level of
    Low -> "low"
    Medium -> "medium"
    High -> "high"
    Critical -> "critical"
