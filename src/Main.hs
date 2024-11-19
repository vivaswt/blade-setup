module Main (main) where

import Control.Monad (replicateM)
import Data.List (sortBy)
import Control.Monad.Trans.State.Lazy

type Width = Int

type Pieace = Int

type NumberOfProduct = Int

type Request = (Width, NumberOfProduct)

main :: IO ()
main = do
  {-   n <- readLn
    rs <- replicateM n readRequests
    print . solve pieaces $ unfoldPairs rs
   -}
  w <- readLn
  print . pattern w $ pieaces

solve :: [Pieace] -> [Width] -> [Pieace]
solve restPS ws = []

-- | 指定された巾に対するピースの組み合わせを返す
pattern ::
  -- | 取りたい巾
  Width ->
  -- | (残りピース)
  [Pieace] ->
  -- | (組合せ結果, 残りピース, )
  (Maybe [Pieace], [Pieace])
pattern _ [] = (Nothing, [])
pattern w (p : ps)
  | p == w = (Just [p], ps)
  | p > w = appendToRest p . pattern w $ ps
  | otherwise =
      let result'@(resultPS', _) = pattern (w - p) ps
       in case resultPS' of
            Just _ -> appendToResult p result'
            _ -> appendToRest p . pattern w $ ps
  where
    appendToResult a (mas, bs) = ((a :) <$> mas, bs)
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