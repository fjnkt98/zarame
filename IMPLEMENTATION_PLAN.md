# 形態素解析器実装計画（ステップバイステップ）

## 概要

このドキュメントは、日本語形態素解析器をZig言語で段階的に実装するための計画書です。
各機能を個別のコミットとして実装し、順次追加していきます。

## 現在の状態

- ✅ **Step 0完了**: PRをクリーンな状態にリセット
- ✅ **Step 1完了**: Dictionary構造の実装
- 📋 **次**: Step 2（Lattice構造）の実装開始

## ベース機能（既に実装済み）

コミット e4f7220時点で以下が実装されています：

1. **ダブル配列トライ (src/trie.zig)**
   - DoubleArray構造体
   - build() - トライ構築
   - search() - 完全一致検索
   - prefixSearch() - 最長一致検索
   - commonPrefixSearch() - 共通接頭辞検索

2. **基本プロジェクト構造**
   - build.zig, build.zig.zon
   - .github/workflows/ci.yaml
   - src/root.zig, src/main.zig

## 実装ステップ

### ✅ Step 1: 辞書構造の実装 (dictionary.zig) - 完了

**目的**: 形態素解析に必要な辞書データ構造を実装

**実装内容**:
- `Entry` 構造体: 辞書エントリ（表層形、左右文脈ID、コスト、品詞情報）
- `ConnectionCosts` 構造体: 連接コスト行列
- `Dictionary` 構造体: 辞書の統合インターフェース

**テスト**:
- ✅ Entryの作成とアクセス
- ✅ ConnectionCostsの初期化とコスト取得/設定  
- ✅ Dictionaryの基本操作

**依存関係**:
- trie.zig (既存)

---
- trie.zig (既存)

---

### Step 2: ラティス構造の実装 (lattice.zig)

**目的**: ビタビアルゴリズムで使用するラティス（格子）構造を実装

**実装内容**:
- `LatticeNode` 構造体: 形態素候補ノード
- `Lattice` 構造体: ラティス構造
- BOS（文頭）/EOS（文末）ノードの管理
- ノードの追加と取得

**テスト**:
- ラティスの作成
- ノードの追加
- EOSノードの追加
- ノード表層形の取得

**依存関係**:
- dictionary.zig (Step 1)

---

### Step 3: ビタビアルゴリズムの実装 (viterbi.zig)

**目的**: 最適な形態素列を見つけるためのビタビアルゴリズムを実装

**実装内容**:
- `Token` 構造体: 解析結果のトークン
- `viterbi()` 関数: ビタビアルゴリズムのメイン処理
- `forwardPass()` 関数: 前向き動的計画法
- `backwardPass()` 関数: 最適パスのバックトラック
- `buildLattice()` 関数: 入力テキストからラティス構築
- `pathToTokens()` 関数: パスからトークン列への変換

**テスト**:
- 簡単なテキストでのトークン化
- 複数の候補がある場合のコスト計算
- 最適パスの選択

**依存関係**:
- dictionary.zig (Step 1)
- lattice.zig (Step 2)

---

### Step 4: バイナリシリアライゼーションの実装 (serialization.zig)

**目的**: 辞書をバイナリ形式で保存・読み込みし、`@embedFile`で実行ファイルに埋め込み可能にする

**実装内容**:
- バイナリフォーマットの定義（ヘッダー構造）
- `serializeDoubleArray()` 関数: ダブル配列のシリアライズ
- `deserializeDoubleArray()` 関数: ダブル配列のデシリアライズ
- `serializeEntries()` 関数: エントリのシリアライズ
- `deserializeEntries()` 関数: エントリのデシリアライズ
- `serializeConnectionCosts()` 関数: 連接コストのシリアライズ
- `deserializeConnectionCosts()` 関数: 連接コストのデシリアライズ
- `serializeDictionary()` 関数: 辞書全体のシリアライズ
- `deserializeDictionary()` 関数: 辞書全体のデシリアライズ

**テスト**:
- 各コンポーネントのシリアライズ/デシリアライズ
- ラウンドトリップテスト（書いて読んで元に戻る）
- `@embedFile`での読み込みテスト

**依存関係**:
- dictionary.zig (Step 1)
- trie.zig (既存)

---

### Step 5: トークナイザーの実装 (tokenizer.zig)

**目的**: 全てのコンポーネントを統合した使いやすいインターフェースを提供

**実装内容**:
- `CharType` 列挙型: 文字種分類（ひらがな、カタカナ、漢字等）
- `classifyChar()` 関数: 文字種判定
- `Tokenizer` 構造体: メインインターフェース
- `tokenize()` メソッド: テキストのトークン化

**テスト**:
- 文字種分類のテスト
- シンプルなテキストのトークン化
- 複雑なテキストのトークン化

**依存関係**:
- dictionary.zig (Step 1)
- lattice.zig (Step 2)
- viterbi.zig (Step 3)

---

### Step 6: 使用例とドキュメント

**目的**: 実装した機能の使い方を示し、ドキュメントを整備

**実装内容**:
- README.md: 日本語での使用ガイド
- examples/complete_example.zig: 完全な使用例
  - 辞書のビルド
  - バイナリへのシリアライズ
  - `@embedFile`での埋め込み
  - トークン化の実行

**テスト**:
- 例の実行確認

**依存関係**:
- 全てのStep 1-5

---

## 実装順序とマイルストーン

| Step | 機能 | PR番号 | ステータス |
|------|------|--------|-----------|
| 1 | Dictionary | TBD | 準備中 |
| 2 | Lattice | TBD | 未着手 |
| 3 | Viterbi | TBD | 未着手 |
| 4 | Serialization | TBD | 未着手 |
| 5 | Tokenizer | TBD | 未着手 |
| 6 | Examples & Docs | TBD | 未着手 |

## 各Stepの完了条件

各Stepは以下の条件を満たして初めて完了とします：

1. ✅ コードが実装されている
2. ✅ ユニットテストが書かれ、全て通過している
3. ✅ `zig build` でビルドが通る
4. ✅ `zig build test` でテストが通る
5. ✅ コードレビューでの指摘事項が解決されている
6. ✅ ドキュメントコメントが書かれている

## 次のアクション

現在のステータス: **Step 0 完了（ベースコミットへのリセット）**

次のステップ: **Step 1: Dictionary の実装を開始**
