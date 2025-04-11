module RiskStatus
  ( RiskStatus(..)
  , parseRiskStatus
  , renderRiskStatus
  ) where

import Data.Char (toLower)

data RiskStatus = Open | Acknowledged | Resolved
  deriving (Eq, Show)

parseRiskStatus :: String -> Either String RiskStatus
parseRiskStatus raw =
  case map toLower raw of
    "open" -> Right Open
    "acknowledged" -> Right Acknowledged
    "resolved" -> Right Resolved
    _ -> Left "Status must be one of: open, acknowledged, resolved."

renderRiskStatus :: RiskStatus -> String
renderRiskStatus status =
  case status of
    Open -> "open"
    Acknowledged -> "acknowledged"
    Resolved -> "resolved"
