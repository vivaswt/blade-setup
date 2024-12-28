{-# HLINT ignore "Use first" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map with tuple-section" #-}

module Main where

import Control.Monad (guard)
import Control.Monad.Trans.State (StateT (StateT, runStateT), get, put)
import Data.List (delete, group, sortBy)
import System.IO (hSetEncoding, stdout, utf8)

type Width = Int

type NumberOfProduct = Int

type Request = (Width, NumberOfProduct)

type NumberOfPieace = Int

-- | ピースの状態
data PieaceStatus
  = PieaceStatus {numberOfBlades :: Int, restPieaces :: [Pieace]}

instance Show PieaceStatus where
  show :: PieaceStatus -> String
  show PieaceStatus {numberOfBlades = nb, restPieaces = rs} =
    "PieaceStatus {blades = "
      ++ show nb
      ++ ", rest = "
      ++ (show . aggregatePieaces $ rs)
      ++ "}"

-- | ピースの種類
data Pieace = Blade | Spacer Int
  deriving (Show, Eq)

-- | ピースの長さを返す
pieaceLength :: Pieace -> Int
pieaceLength Blade = 10
pieaceLength (Spacer w) = w

main :: IO ()
main = do
  hSetEncoding stdout utf8

  putStrLn "スリットパターンの組合せ数を入力してください"
  n <- readLn

  requestPairs <- readSlitPatterns n

  putStrLn "原紙の巾を入力してください"
  paperWidth <- readLn

  putStrLn "シャフト全長を入力してください"
  totalWidth <- readLn

  putStrLn "最後の固定スペーサーの組合せを入力してください"
  putStrLn "例: 5 7 9"
  fixedSpacers <- readInts

  let (pre, post) =
        calculateSlitMargins totalWidth paperWidth requestPairs (sum fixedSpacers)
      slitWidths = unfoldPairs requestPairs
      st = do
        fixedSpacers' <- mconcatMapM selectSpacerByWidth fixedSpacers
        preSpacers <- selectSpacersByWidth (pre - pieaceLength Blade)
        preBlade <- selectBlade
        slitPieaces <- mconcatMapM selectPieacesForSlit slitWidths
        postSpacers <- selectSpacersByWidth post
        return . concat $ [preSpacers, preBlade, slitPieaces, postSpacers, fixedSpacers']
      result =
        runStateT
          st
          PieaceStatus {numberOfBlades = 14, restPieaces = allSpacers}

  case result of
    Nothing -> putStrLn "組合せが見つかりません"
    Just ps -> print . aggregatePieaces . fst $ ps

-- | 指定されたスリット巾に対するピースの組み合わせを返す
selectPieacesForSlit :: Width -> StateT PieaceStatus Maybe [Pieace]
selectPieacesForSlit w = do
  spacers <- selectSpacersByWidth w'
  blade <- selectBlade
  return $ spacers ++ blade
  where
    w' = w - pieaceLength Blade

-- | 指定された巾に対するスペーサーの組み合わせを返す
selectSpacersByWidth :: Width -> StateT PieaceStatus Maybe [Pieace]
selectSpacersByWidth = StateT . pattern

-- | 指定された巾に対するスペーサーの組み合わせを返す
pattern ::
  -- | 取り出すの巾
  Width ->
  -- | 残りピース
  PieaceStatus ->
  -- | 取り出したスペーサーの組み合わせと残りピース
  Maybe ([Pieace], PieaceStatus)
pattern _ (PieaceStatus {restPieaces = []}) = Nothing
pattern w pStatus@(PieaceStatus {restPieaces = (p : ps)})
  | pieaceLength p == w = return ([p], pStatus {restPieaces = ps})
  | pieaceLength p > w = do
      (result', pStatus') <- pattern w pStatus {restPieaces = ps}
      return
        ( result',
          pStatus'
            { restPieaces = p : restPieaces pStatus'
            }
        )
  | otherwise =
      let result =
            pattern (w - pieaceLength p) pStatus {restPieaces = ps}
       in case result of
            Just _ -> appendToResult p <$> result
            _ -> appendToRest p <$> pattern w pStatus {restPieaces = ps}
  where
    appendToResult p' (results, pieaceStatus) =
      (p' : results, pieaceStatus)
    appendToRest p' (results, pieaceStatus) =
      ( results,
        pieaceStatus {restPieaces = p' : restPieaces pieaceStatus}
      )

-- | 下刃を一枚とる
--
-- >>> runStateT selectBlade (PieaceStatus 2 [Spacer 10, Spacer 5])
-- Just ([Blade],PieaceStatus {blades = 1, rest = [(Spacer 10,1),(Spacer 5,1)]})
--
-- >>> runStateT selectBlade (PieaceStatus 0 [Spacer 10, Spacer 5])
-- Nothing
selectBlade :: StateT PieaceStatus Maybe [Pieace]
selectBlade = do
  pStatus <- get
  let nb = numberOfBlades pStatus
  guard (nb > 0)
  put pStatus {numberOfBlades = nb - 1}
  return [Blade]

-- | 指定されたスペーサーを一枚とる
--
-- >>> runStateT (selectSpacerByWidth 5) (PieaceStatus 2 [Spacer 10, Spacer 5, Spacer 5, Spacer 2])
-- Just ([Spacer 5],PieaceStatus {blades = 2, rest = [(Spacer 10,1),(Spacer 5,1),(Spacer 2,1)]})
--
-- >>> runStateT (selectSpacerByWidth 7) (PieaceStatus 2 [Spacer 10, Spacer 5, Spacer 5, Spacer 2])
-- Nothing
selectSpacerByWidth :: Width -> StateT PieaceStatus Maybe [Pieace]
selectSpacerByWidth w = do
  pStatus <- get
  guard (Spacer w `elem` restPieaces pStatus)
  put pStatus {restPieaces = delete (Spacer w) (restPieaces pStatus)}
  return [Spacer w]

-- | スリット部前後の巾構成を返す
--
-- >>> calculateSlitMargins 100 80 [(5, 2), (4, 5)] 5
-- (10,55)
--
-- >>> calculateSlitMargins 100 80 [(20,4)] 0
-- (10,10)
--
-- >>> calculateSlitMargins 101 80 [(20,4)] 5
-- (10,6)
calculateSlitMargins ::
  -- | シャフトロール全長
  Width ->
  -- | 原紙巾
  Width ->
  -- | スリットパターン
  [Request] ->
  -- | 最後の固定スペーサーの巾
  Width ->
  (Width, Width)
calculateSlitMargins
  totalWidth
  paperWidth
  rgs
  fixedWidth =
    (pre, post)
    where
      rest = totalWidth - paperWidth
      pre = roundBy5 (fromIntegral rest / 2)
      slitWidth = sum (map (uncurry (*)) rgs)
      post = totalWidth - pre - slitWidth - fixedWidth

-- | スリットパターンの巾と数を指定された回数分だけ標準入力から読み込む
readSlitPatterns :: Int -> IO [Request]
readSlitPatterns n = mapM readWithMessage [1 .. n]
  where
    readWithMessage i = do
      putStrLn $ "スリットパターンの巾と数を入力してください(" ++ show i ++ "組目)"
      putStrLn "例: 250巾を4つの場合⇒ 250 4"
      readRequest

-- | 四捨五入
--
-- >>> myRound 3.1
-- 3
--
-- >>> myRound 3.5
-- 4
myRound ::
  (RealFrac a1, Integral a2) =>
  a1 ->
  a2
myRound x
  | n <= -0.5 = m - 1
  | n >= 0.5 = m + 1
  | otherwise = m
  where
    (m, n) = properFraction x

-- | 5の倍数に丸める
--
-- >>> roundBy5 3.1
-- 5
--
-- >>> roundBy5 3.5
-- 5
--
-- >>> roundBy5 3.6
-- 5
roundBy5 :: (RealFrac a1, Integral a) => a1 -> a
roundBy5 x = 5 * myRound (x / 5)

-- | ペアのリストを展開する
--
-- >>> unfoldPairs [(5, 2), (7, 3)]
-- [5,5,7,7,7]
unfoldPairs :: [(a, Int)] -> [a]
unfoldPairs = concatMap (\(e, n) -> replicate n e)

-- | スペーサーのリスト
allSpacers :: [Pieace]
allSpacers = map Spacer . unfoldPairs . sortBy (flip compare) $ pairs
  where
    pairs = [(5, 14), (7, 15), (9, 1), (10, 19), (20, 20), (50, 19), (100, 5)]

-- | リストの各要素に対して関数を適用し、結果をモノイドで結合する
mconcatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
mconcatMapM f as = mconcat <$> mapM f as

-- | 整数のリストを標準入力から読み込む
readInts :: IO [Int]
readInts = map read . words <$> getLine

-- | リクエストを標準入力から読み込む
readRequest :: IO Request
readRequest = (\(w : n : _) -> (w, n)) <$> readInts

-- | ピースを種類別に集計する
--
-- >>> aggregatePieaces [Spacer 10, Spacer 10, Spacer 5, Spacer 5, Spacer 5]
-- [(Spacer 10,2),(Spacer 5,3)]
aggregatePieaces :: [Pieace] -> [(Pieace, NumberOfPieace)]
aggregatePieaces = map (\ps -> (head ps, length ps)) . group
