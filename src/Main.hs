{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
module Main (main) where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (StateT (StateT, runStateT))
import Data.List (group, sortBy)

type Width = Int

type Pieace = Int

type NumberOfProduct = Int

type Request = (Width, NumberOfProduct)

type NumberOfPieace = Int

data PieaceStatus
  = PieaceStatus {numberOfBlades :: Int, restPieaces :: [Pieace]}

instance Show PieaceStatus where
  show PieaceStatus {numberOfBlades = nb, restPieaces = rs} =
    "PieaceStatus {blades = "
      ++ show nb
      ++ ", rest = "
      ++ (show . aggregatePieaces $ rs)
      ++ "}"

main :: IO ()
main = do
  n <- readLn
  rs <- replicateM n readRequests
  let ws = unfoldPairs rs
      st = mconcatMapM pattern ws
      result =
        runStateT
          st
          PieaceStatus {numberOfBlades = 0, restPieaces = pieaces}
  print . fmap (\(r, s) -> (aggregatePieaces r, s)) $ result

solve :: [Pieace] -> [Width] -> Maybe [Pieace]
solve restPS ws = Nothing

pattern :: Width -> StateT PieaceStatus Maybe [Pieace]
pattern = StateT . pattern'

-- | 指定された巾に対するピースの組み合わせを返す
pattern' ::
  -- | 取りたい巾
  Width ->
  -- | 残りピース
  PieaceStatus ->
  -- | Maybe (組合せ結果, 残りピース)
  Maybe ([Pieace], PieaceStatus)
pattern' _ (PieaceStatus {restPieaces = []}) = Nothing
pattern' w pStatus@(PieaceStatus {restPieaces = (p : ps)})
  | p == w = return ([p], pStatus {restPieaces = ps})
  | p > w = do
      (result', pStatus') <- pattern' w pStatus {restPieaces = ps}
      return
        ( result',
          pStatus'
            { restPieaces = p : restPieaces pStatus'
            }
        )
  | otherwise =
      let result = pattern' (w - p) pStatus {restPieaces = ps}
       in case result of
            Just _ -> appendToResult p <$> result
            _ -> appendToRest p <$> pattern' w pStatus {restPieaces = ps}
  where
    appendToResult p' (results, pieaceStatus) = (p' : results, pieaceStatus)
    appendToRest p' (results, pieaceStatus) =
      ( results,
        pieaceStatus {restPieaces = p' : restPieaces pieaceStatus}
      )

unfoldPairs :: [(a, Int)] -> [a]
unfoldPairs = concatMap (\(e, n) -> replicate n e)

pieaces :: [Pieace]
pieaces = unfoldPairs . sortBy (flip compare) $ pairs
  where
    pairs = [(5, 14), (7, 15), (10, 19), (20, 20), (50, 19), (100, 5)]

mconcatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
mconcatMapM f as = mconcat <$> mapM f as

readInts :: IO [Int]
readInts = map read . words <$> getLine

readRequests :: IO Request
readRequests = (\(w : n : _) -> (w, n)) <$> readInts

aggregatePieaces :: [Pieace] -> [(Pieace, NumberOfPieace)]
aggregatePieaces = map (\ps -> (head ps, length ps)) . group

aggregatetResult :: ([Pieace], [Pieace]) -> ([(Pieace, NumberOfPieace)], [(Pieace, NumberOfPieace)])
aggregatetResult (result, rest) = (aggregatePieaces result, aggregatePieaces rest)
