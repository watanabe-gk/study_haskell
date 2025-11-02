# Haskell標準ライブラリのソートアルゴリズム解説

## Data.List.sort の内部実装

Haskellの `Data.List.sort` は **MergeSort（マージソート）** をベースにした高度に最適化された実装です。

## 実装の詳細

### 1. アルゴリズム: Natural MergeSort

Haskellのソートは「Natural MergeSort」と呼ばれる、標準的なマージソートの改良版を使用しています。

```haskell
-- GHC.List の実際の実装（簡略版）
sort :: Ord a => [a] -> [a]
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a] xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as) : sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = as [a] : sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = merge a b : mergePairs xs
    mergePairs xs       = xs
```

### 2. 主な特徴

#### (1) 安定ソート (Stable Sort)
- 同じ値を持つ要素の相対的な順序が保たれる
- JavaScriptのTimsortと同様

#### (2) Natural（自然な）ソート
- データ内の既にソート済みの部分列（run）を検出
- 昇順または降順の連続した要素をまとめて処理
- 部分的にソート済みのデータに対して非常に効率的

#### (3) 遅延評価の活用
- Haskellの遅延評価により、必要な部分だけを計算
- 無限リストの一部をソートすることも可能

### 3. 時間計算量

| ケース | 計算量 |
|--------|--------|
| 最良   | O(n) - 既にソート済みの場合 |
| 平均   | O(n log n) |
| 最悪   | O(n log n) |
| 空間   | O(n) |

### 4. 他の言語との比較

| 言語/環境 | アルゴリズム | 特徴 |
|-----------|-------------|------|
| **Haskell** | Natural MergeSort | 安定、遅延評価対応 |
| **JavaScript (V8)** | Timsort | MergeSort + InsertionSort のハイブリッド |
| **Python** | Timsort | 同上、実データで高速 |
| **Java** | Timsort (オブジェクト)<br>Dual-Pivot QuickSort (プリミティブ) | 型によって使い分け |
| **C++ std::sort** | Introsort | QuickSort + HeapSort + InsertionSort |
| **Go** | pdqsort | Pattern-defeating QuickSort |

### 5. Natural MergeSort vs Timsort

#### 共通点
- どちらもMergeSortベース
- 既存のソート済み部分列（run）を検出して利用
- 安定ソート

#### 違い
| 特徴 | Natural MergeSort | Timsort |
|------|------------------|---------|
| 小さいrunの処理 | そのままマージ | InsertionSortで最小サイズまで拡張 |
| runの最小サイズ | 制約なし | 通常32～64要素 |
| ギャロッピングモード | なし | あり（連続した要素の効率的な検索） |
| 最適化の複雑さ | シンプル | より複雑で実データ向け |

### 6. コード例: 実際の動作

```haskell
-- 部分的にソート済みのデータ
let data1 = [1,2,3,4,5] ++ [10,9,8,7,6] ++ [11,12,13]

-- sequences が検出する run:
-- [1,2,3,4,5]      -- 昇順のrun
-- [10,9,8,7,6]     -- 降順のrun（反転される）
-- [11,12,13]       -- 昇順のrun

-- これらを効率的にマージ
```

### 7. なぜMergeSortなのか？

Haskellが他のアルゴリズムではなくMergeSortを選んだ理由：

1. **不変データ構造との相性**
   - Haskellのリストは不変（immutable）
   - In-placeな操作（QuickSortなど）は非効率

2. **安定性の保証**
   - 関数型プログラミングでは予測可能性が重要
   - 安定ソートは動作が予測しやすい

3. **遅延評価との親和性**
   - リストを少しずつ生成・消費できる
   - メモリ効率が良い

4. **worst-caseの保証**
   - 常にO(n log n)が保証される
   - QuickSortの最悪ケースO(n²)を回避

### 8. 最適化テクニック

GHCの実装には以下の最適化が含まれています：

1. **Fusion（融合）最適化**
   - 中間リストの生成を削減
   - ストリーム処理のような効率

2. **特殊化（Specialization）**
   - 具体的な型に対して最適化されたコード生成
   - Int, Char などの基本型は特に高速

3. **インライン展開**
   - 小さな関数呼び出しのオーバーヘッド削減

## まとめ

Haskellの標準ソートは：
- **Natural MergeSort** を使用
- **安定で予測可能**
- **遅延評価を活用**
- **部分的にソート済みのデータに効率的**
- JavaScriptのTimsortと似ているが、Haskellの特性に最適化されている

実用上は、JavaScript/Pythonと同等かそれ以上の性能を発揮します。
