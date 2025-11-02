import System.CPUTime
import Text.Printf
import Data.List (sort)

-- ===== QuickSort =====
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    quickSort [y | y <- xs, y < x]
    ++ [x] ++
    quickSort [y | y <- xs, y >= x]

-- ===== MergeSort =====
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- ===== InsertionSort =====
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : insert x ys

-- ===== BubbleSort =====
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = if xs == xs' then xs else bubbleSort xs'
  where
    xs' = bubble xs

bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs)
    | x > y     = y : bubble (x:xs)
    | otherwise = x : bubble (y:xs)

-- ===== Timing Function =====
timeIt :: (a -> b) -> a -> IO (b, Double)
timeIt f x = do
    start <- getCPUTime
    let result = f x
    result `seq` return ()
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    return (result, diff)

-- ===== Benchmark Function =====
benchmark :: Ord a => String -> ([a] -> [a]) -> [a] -> IO ()
benchmark name sortFunc xs = do
    (_, time) <- timeIt sortFunc xs
    printf "%-20s: %.6f 秒\n" name time

-- ===== Main =====
main :: IO ()
main = do
    let sizes = [100, 1000, 5000]

    mapM_ (\size -> do
        putStrLn $ "\n=== リストサイズ: " ++ show size ++ " ==="
        let testData = reverse [1..size] :: [Int]

        benchmark "QuickSort" quickSort testData
        benchmark "MergeSort" mergeSort testData
        benchmark "InsertionSort" insertionSort testData
        benchmark "BubbleSort" bubbleSort testData
        benchmark "標準ライブラリ" sort testData
        ) sizes

    putStrLn "\n=== アルゴリズムの特徴 ==="
    putStrLn "QuickSort     : 平均 O(n log n), 最悪 O(n²)"
    putStrLn "MergeSort     : 常に O(n log n), 安定ソート"
    putStrLn "InsertionSort : 平均 O(n²), 小さいデータに効率的"
    putStrLn "BubbleSort    : 平均 O(n²), 教育用途"
    putStrLn "標準ライブラリ: Haskellは高度に最適化されたソートを使用"
