module Main (main) where

import Control.Monad (replicateM)
import Data.List (sort, sortBy)

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
  print . pattern pieaces $ w

solve :: [Pieace] -> [Width] -> [Pieace]
solve _ [] = []

-- | 指定された巾に対するピースの組み合わせを返す
pattern ::
  -- | (残りピース)
  [Pieace] ->
  -- | 取りたい巾
  Width ->
  -- | (組合せ結果, 残りピース)
  Maybe ([Pieace], [Pieace])
pattern [] _ = Nothing
pattern (p : ps) w
  | p == w = Just ([p], ps)
  | p > w = appendToRest p <$> pattern ps w
  | otherwise =
      let result = pattern ps (w - p)
       in case result of
            Just _ -> appendToResult p <$> result
            _ -> appendToRest p <$> pattern ps w
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