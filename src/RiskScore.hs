module RiskScore
  ( riskScore
  ) where

import RiskTypes (RiskLevel(..))

riskScore :: RiskLevel -> Int
riskScore level =
  case level of
    Low -> 25
    Medium -> 50
    High -> 75
    Critical -> 95
