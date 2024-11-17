module Main (main) where

import Control.Monad (replicateM)
import Data.List (sortBy)

type Width = Int

type Pieace = Int

type NumberOfProduct = Int

type Request = (Width, NumberOfProduct)

main :: IO ()
main = do
  n <- readLn
  rs <- replicateM n readRequests
  print . solve pieaces $ rs

solve :: [Pieace] -> [Request] -> [Pieace]
solve ps rs = []
  where
    rs' = unfoldPairs rs

-- | 指定された巾に対するピースの組み合わせを返す
pattern ::
  -- | (残りピース, 使えなかったピース)
  ([Pieace], [Pieace]) ->
  -- | 取りたい巾
  Width ->
  -- | (組合せ結果, (残りピース, 使えなかったピース))
  ([Pieace], ([Pieace], [Pieace]))
pattern ([], unmatchedPieaces) _ = ([], ([], unmatchedPieaces))
pattern (p : restPieaces, unmatchedPieaces) w
  | p == w = ([p], (restPieaces, unmatchedPieaces))
  | p > w = pattern (restPieaces, unmatchedPieaces ++ [p]) w
  | otherwise =
      let (resultPieaces, patternState) =
            pattern (restPieaces, unmatchedPieaces) (w - p)
       in if null resultPieaces
            then pattern (restPieaces, unmatchedPieaces ++ [p]) w
            else (p : resultPieaces, patternState)

unfoldPairs :: [(a, Int)] -> [a]
unfoldPairs = concatMap (\(e, n) -> replicate n e)

pieaces :: [Pieace]
pieaces = unfoldPairs . sortBy (flip compare) $ pairs
  where
    pairs = [(5, 14), (7, 15), (10, 19), (20, 20), (50, 19), (100, 5)]

readInts :: IO [Int]
readInts = map read . words <$> getLine

readRequests :: IO Request
readRequests = (\(w : n : _) -> (w, n)) <$> readInts