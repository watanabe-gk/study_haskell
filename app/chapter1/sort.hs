-- クイックソート（昇順）
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    qsort [y | y <- xs, y < x]
    ++ [x] ++
    qsort [y | y <- xs, y >= x]

-- クイックソート（降順）
qsortDesc :: Ord a => [a] -> [a]
qsortDesc [] = []
qsortDesc (x:xs) =
    qsortDesc [y | y <- xs, y > x]
    ++ [x] ++
    qsortDesc [y | y <- xs, y <= x]

-- Main function for testing
main :: IO ()
main = do
    let numbers = [3,1,4,1,5,9,2,6]

    putStrLn "=== クイックソート（昇順） ==="
    putStrLn $ "元のリスト: " ++ show numbers
    putStrLn $ "昇順ソート: " ++ show (qsort numbers)

    putStrLn ""
    putStrLn "=== クイックソート（降順） ==="
    putStrLn $ "元のリスト: " ++ show numbers
    putStrLn $ "降順ソート: " ++ show (qsortDesc numbers)

    putStrLn ""
    putStrLn "=== その他の例 ==="
    putStrLn $ "qsort \"hello\" = " ++ qsort "hello"
    putStrLn $ "qsortDesc \"hello\" = " ++ qsortDesc "hello"