# Zarame - Zig言語による日本語形態素解析器

Zigで実装された日本語形態素解析器です。ダブル配列とビタビアルゴリズムを使用して、効率的な日本語テキストのトークン化を実現します。

## 概要

このプロジェクトは、MeCabやKagomeのような日本語形態素解析器をZig言語で実装したものです。以下の主要コンポーネントが実装されています：

### 実装済みの機能

1. **ダブル配列トライ** (`src/trie.zig`)
   - ✅ 高速な辞書検索
   - ✅ 省メモリなダブル配列実装
   - ✅ 共通接頭辞検索と完全一致検索

2. **辞書構造** (`src/dictionary.zig`)
   - ✅ エントリ構造（表層形、左右文脈ID、単語コスト、品詞情報）
   - ✅ 連接コスト行列
   - ✅ 辞書検索インターフェース

3. **ラティス構造** (`src/lattice.zig`)
   - ✅ 形態素候補の格子表現
   - ✅ ノードのコスト管理
   - ✅ 文頭(BOS)・文末(EOS)マーカー

4. **ビタビアルゴリズム** (`src/viterbi.zig`)
   - ✅ 動的計画法による最適パス探索
   - ✅ 前向きパス：累積コスト計算
   - ✅ 後ろ向きパス：最適パスのバックトラック
   - ✅ トークン列の生成

5. **トークナイザー** (`src/tokenizer.zig`)
   - ✅ テキスト分割のメインインターフェース
   - ✅ 文字種分類（ひらがな、カタカナ、漢字等）
   - ✅ 未知語処理の基盤

6. **バイナリ形式** (`src/serialization.zig`)
   - ✅ 辞書のシリアライズ・デシリアライズ
   - ✅ `@embedFile`でバイナリに内包可能
   - ✅ コンパクトなバイナリフォーマット

## ダブル配列をバイナリにする方法

ご質問の「ダブル配列として構築した辞書をバイナリにする方法」について、以下の手順で実現できます：

### 1. 辞書のビルド

```zig
const std = @import("std");
const zarame = @import("zarame");

pub fn buildDictionary() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // 1. 表層形からダブル配列を構築
    const keywords = [_][]const u8{
        "東京",
        "に",
        "行く",
    };

    var da = try zarame.DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();

    // 2. 辞書エントリを作成
    var entries = std.ArrayList(zarame.Entry).init(allocator);
    defer entries.deinit();

    const features1 = [_][]const u8{ "名詞", "固有名詞", "地名" };
    try entries.append(zarame.Entry.init("東京", 1, 1, 3000, &features1));

    // 3. 連接コストを作成
    var costs = try zarame.ConnectionCosts.init(allocator, 100, 100);
    defer costs.deinit();

    // 4. バイナリファイルにシリアライズ
    const file = try std.fs.cwd().createFile("dict.bin", .{});
    defer file.close();

    try zarame.serialization.serializeDictionary(
        file.writer(),
        &da,
        entries.items,
        &costs,
    );
}
```

### 2. バイナリをアプリケーションに内包

Kagomeのように、辞書をバイナリに内包するには：

```zig
// dict.binをコンパイル時に埋め込み
const dict_data = @embedFile("dict.bin");

pub fn loadDictionary(allocator: std.mem.Allocator) !zarame.Dictionary {
    // バイナリからデシリアライズ
    const result = try zarame.serialization.deserializeDictionary(
        allocator,
        dict_data,
    );

    return zarame.Dictionary.init(
        allocator,
        &result.da,
        result.entries,
        result.costs,
    );
}
```

### 3. 使用例

```zig
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // 埋め込まれた辞書をロード
    var dictionary = try loadDictionary(allocator);
    defer dictionary.deinit();

    // トークナイザーを作成
    var tokenizer = zarame.Tokenizer.init(allocator, dictionary);
    defer tokenizer.deinit();

    // テキストを分析
    const text = "東京に行く";
    const tokens = try tokenizer.tokenize(text);
    defer allocator.free(tokens);

    // 結果を出力
    for (tokens) |token| {
        std.debug.print("{s}\t", .{token.surface});
        for (token.features) |feature| {
            std.debug.print("{s},", .{feature});
        }
        std.debug.print("\n", .{});
    }
}
```

## バイナリフォーマットの詳細

実装したバイナリ形式は以下の構造です：

```
ヘッダー（固定サイズ）:
- マジックナンバー (4バイト): 0x4D524157
- バージョン (2バイト): 1
- エントリ数 (4バイト)
- ダブル配列サイズ (4バイト × 2)
- 連接コスト行列サイズ (2バイト × 2)
- セクションオフセット (8バイト × 3)

ダブル配列セクション:
- base配列サイズ + データ
- check配列サイズ + データ

エントリセクション:
- 各エントリの表層形、文脈ID、コスト、品詞情報

連接コストセクション:
- 行列サイズ + コストデータ
```

## 形態素解析器を完成させるためのステップ

### ✅ 完了済み

1. ✅ ダブル配列実装（既存）
2. ✅ 辞書データ構造
3. ✅ ラティス構造
4. ✅ ビタビアルゴリズム
5. ✅ バイナリシリアライゼーション
6. ✅ トークナイザーインターフェース

### 🔄 今後の実装

1. ⬜ CSVから辞書をビルドするツール
2. ⬜ 未知語処理の完全実装
3. ⬜ 実際の辞書（MeCab辞書等）での動作確認
4. ⬜ パフォーマンス最適化
5. ⬜ より詳細なドキュメント

## ビルドとテスト

```bash
# ライブラリをビルド
zig build

# テストを実行
zig build test

# サンプルを実行
zig build run
```

## 実装方針

### メモリ管理

- アロケータを明示的に渡す設計
- `defer`を使用した確実なメモリ解放
- 辞書データは一度ロードして再利用

### パフォーマンス

- ダブル配列による高速な辞書検索（O(m)、mはクエリ長）
- 動的計画法による効率的なビタビアルゴリズム
- バイナリ形式によるI/Oの最小化

### 拡張性

- モジュール化された設計
- 各コンポーネントが独立してテスト可能
- カスタム辞書フォーマットへの対応が容易

## 参考文献

- MeCab: https://taku910.github.io/mecab/
- Kagome: https://github.com/ikawaha/kagome
- ダブル配列: Aoe, J. (1989). "An Efficient Digital Search Algorithm by Using a Double-Array Structure"

## ライセンス

LICENSEファイルを参照してください。

## 詳細ドキュメント

より詳しい技術的な説明は `IMPLEMENTATION_GUIDE.md`（英語）を参照してください。
