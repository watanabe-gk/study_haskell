-- 3章の練習問題 1
-- [Char] または String
ex1 :: [Char]
ex1 = ['a', 'b', 'c']

-- (Char, Char, Char)
ex2 :: (Char, Char, Char)
ex2 = ('a', 'b', 'c')

-- [(Bool, Char)]
ex3 :: [(Bool, Char)]
ex3 = [(False, '0'), (True, '1')]

-- ([Bool], [Char])
ex4 :: ([Bool], [Char])
ex4 = ([False, True], ['0', '1'])

-- [[a] -> [a]]
ex5 :: [[a] -> [a]]
-- ex5 = [tail, init, reverse]
ex5 = [drop 1, init, reverse]

-- 3章の練習問題 2
bools :: [Bool]
bools = [True, False, True]

nums :: [[Int]]
nums = [[1,2], [3,4]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- 3章の練習問題 3
second :: [a] -> a
-- second xs = head (tail xs)
second xs = head (drop 1 xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

{-
関数がEqクラスのインスタンスになれない理由:

1. 一般的な関数の等価性判定は計算不可能（停止性問題と同等）
   - 無限の入力すべてで同じ出力を返すか検証する必要がある

2. 実現可能なケース:
   - 有限ドメイン: 入力が有限個なら全パターンテスト可能
   - データ構造表現: 式として表現すれば構造的比較可能
   - メモ化/テーブル: Map等で実装すれば比較可能

結論: 一般的な関数 (a -> b) はEqのインスタンスにできない
-}