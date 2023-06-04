module Lib
    ( p
    , sequence1
    , sequence2
    , sequence3
    , sequence4
    ) where


p :: Double
p = (1 + sqrt 13) / 2

pSeq :: Int -> Double
pSeq 1 = 1
pSeq n = sqrt (3 + pSeq (n - 1))

pSeqRatio :: Int -> Double
pSeqRatio n =
  let
    numerator = pSeq (n + 1) - p
    denominator = pSeq n - p
  in
    abs (numerator / denominator)

aitkenSeq :: Int -> Double
aitkenSeq n =
  let
    numerator = (pSeq (n + 1) - pSeq n)^2
    denominator = pSeq (n + 2) - (2 * pSeq (n + 1)) + pSeq n
  in
    pSeq n - (numerator / denominator)

aitkenSeqRatio :: Int -> Double
aitkenSeqRatio n =
  let
    numerator = aitkenSeq (n + 1) - p
    denominator = aitkenSeq n - p
  in
    abs (numerator / denominator)

sequence1 :: [Double]
sequence1 = map pSeq [1..10]

sequence2 :: [Double]
sequence2 = map pSeqRatio [1..9]

sequence3 :: [Double]
sequence3 = map aitkenSeq [1..8]

sequence4 :: [Double]
sequence4 = map aitkenSeqRatio [1..7]
