-- リストの合計値を計算
sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- リストの積算を計算
productList :: Num a => [a] -> a
productList [] = 1
productList (x:xs) = x * productList xs

-- Main function for testing
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 5]

    putStrLn "=== 合計値と積算の計算 ==="
    putStrLn $ "リスト: " ++ show numbers
    putStrLn $ "合計値: " ++ show (sumList numbers)
    putStrLn $ "積算: " ++ show (productList numbers)