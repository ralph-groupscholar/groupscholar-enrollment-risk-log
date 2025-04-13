module RiskAging
  ( AgeBucket(..)
  , bucketForDays
  , renderAgeBucket
  ) where

data AgeBucket
  = Under7
  | Under14
  | Under30
  | Over30
  deriving (Eq, Show)

bucketForDays :: Int -> AgeBucket
bucketForDays days
  | days <= 7 = Under7
  | days <= 14 = Under14
  | days <= 30 = Under30
  | otherwise = Over30

renderAgeBucket :: AgeBucket -> String
renderAgeBucket bucket =
  case bucket of
    Under7 -> "0-7d"
    Under14 -> "8-14d"
    Under30 -> "15-30d"
    Over30 -> "31d+"
