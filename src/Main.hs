module Main (main) where

import Control.Monad (replicateM)
import Data.List (group, sortBy)

type Width = Int

type Pieace = Int

type NumberOfProduct = Int

type Request = (Width, NumberOfProduct)

type NumberOfPieace = Int

main :: IO ()
main = do
  {-   n <- readLn
    rs <- replicateM n readRequests
    print . solve pieaces $ unfoldPairs rs
   -}
  w <- readLn
  print $ aggregatetResult <$> pattern w pieaces

solve :: [Pieace] -> [Width] -> [Pieace]
solve restPS ws = []

-- | 指定された巾に対するピースの組み合わせを返す
pattern ::
  -- | 取りたい巾
  Width ->
  -- | [残りピース]
  [Pieace] ->
  -- | Maybe (組合せ結果, 残りピース)
  Maybe ([Pieace], [Pieace])
pattern _ [] = Nothing
pattern w (p : ps)
  | p == w = return ([p], ps)
  | p > w = do
      (result, rest) <- pattern w ps
      return (result, p : rest)
  | otherwise =
      let result = pattern (w - p) ps
       in case result of
            Just _ -> appendToResult p <$> result
            _ -> appendToRest p <$> pattern w ps
  where
    appendToResult a (as, bs) = (a : as, bs)
    appendToRest b (as, bs) = (as, b : bs)

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

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

aggregatePieaces :: [Pieace] -> [(Pieace, NumberOfPieace)]
aggregatePieaces = map (\ps -> (head ps, length ps)) . group

aggregatetResult :: ([Pieace], [Pieace]) -> ([(Pieace, NumberOfPieace)], [(Pieace, NumberOfPieace)])
aggregatetResult (result, rest) = (aggregatePieaces result, aggregatePieaces rest)