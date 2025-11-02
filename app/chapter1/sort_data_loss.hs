-- クイックソート（昇順）
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    qsort [y | y <- xs, y < x]
    ++ [x] ++
    qsort [y | y <- xs, y > x] -- 等しい値が除外されてデータ損失

-- Main function for testing
main :: IO ()
main = do
    let numbers = [2,2,3,1,1]

    putStrLn "=== クイックソート（昇順） ==="
    putStrLn $ "元のリスト: " ++ show numbers
    putStrLn $ "昇順ソート: " ++ show (qsort numbers)