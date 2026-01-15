# Zarame Architecture Diagram

## System Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                     User Application                            │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Tokenizer (tokenizer.zig)                     │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │  tokenize(text: []const u8) -> []Token                     │ │
│  │  - Character type classification                           │ │
│  │  - Lattice building                                        │ │
│  │  - Viterbi execution                                       │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
                              │
                ┌─────────────┼─────────────┐
                ▼             ▼             ▼
    ┌──────────────┐ ┌───────────┐ ┌──────────────┐
    │  Dictionary  │ │  Lattice  │ │   Viterbi    │
    │ (dict.zig)   │ │(latt.zig) │ │ (vit.zig)    │
    └──────────────┘ └───────────┘ └──────────────┘
            │
    ┌───────┴───────┐
    ▼               ▼
┌────────┐   ┌─────────────┐
│ Double │   │ Connection  │
│ Array  │   │   Costs     │
│(trie)  │   │   Matrix    │
└────────┘   └─────────────┘
```

## Data Flow

```
Input Text: "東京に行く"
     │
     ▼
┌─────────────────────────────────────┐
│ 1. Dictionary Lookup                │
│    - Use double-array trie          │
│    - Find all possible morphemes    │
└─────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────┐
│ 2. Build Lattice                    │
│                                     │
│  BOS → [東京] → [に] → [行く] → EOS │
│    ↘   [東]                         │
│      ↘ [京]                         │
└─────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────┐
│ 3. Viterbi Algorithm                │
│    Forward Pass:                    │
│    - Calculate min cumulative cost  │
│    - cost = prev_cost +             │
│            connection_cost +        │
│            word_cost                │
│                                     │
│    Backward Pass:                   │
│    - Backtrack from EOS to BOS     │
│    - Follow minimum cost path       │
└─────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────┐
│ 4. Generate Tokens                  │
│    [東京: 名詞,固有名詞,地名]        │
│    [に: 助詞,格助詞]                 │
│    [行く: 動詞,自立]                 │
└─────────────────────────────────────┘
```

## Binary Dictionary Format

```
┌─────────────────────────────────────────────────────┐
│                    HEADER (固定サイズ)                 │
├─────────────────────────────────────────────────────┤
│  Magic Number: 0x4D524157                           │
│  Version: 1                                         │
│  Entry Count: N                                     │
│  DA Base Size: M                                    │
│  DA Check Size: M                                   │
│  Connection Matrix Size: F×B                        │
│  Section Offsets: [da_off, entries_off, conn_off]  │
└─────────────────────────────────────────────────────┘
                      │
        ┌─────────────┼─────────────┐
        ▼             ▼             ▼
┌──────────────┐ ┌──────────┐ ┌────────────┐
│ Double-Array │ │ Entries  │ │Connection  │
│   Section    │ │ Section  │ │   Costs    │
├──────────────┤ ├──────────┤ ├────────────┤
│ • base[]     │ │ • surface│ │ • forward  │
│ • check[]    │ │ • left_id│ │   size     │
│              │ │ • right  │ │ • backward │
│              │ │ • cost   │ │   size     │
│              │ │ • POS    │ │ • matrix[] │
└──────────────┘ └──────────┘ └────────────┘
```

## Viterbi Algorithm Detail

```
Position:    0        3        6        9       12
             │        │        │        │        │
Text:        東       京       に       行       く

Lattice:
             BOS
             /│\
            / │ \
           /  │  \
          ▼   ▼   ▼
         [東京] [東] [京]
          │    └──┬─┘
          │       │
          ▼       ▼
         [に]    [...]
          │
          ▼
        [行く]
          │
          ▼
         EOS

Forward Pass (各ノードの最小コストを計算):
  cost[node] = min(cost[prev] + conn_cost(prev→node) + word_cost[node])

Backward Pass (最適パスをたどる):
  EOS ← [行く] ← [に] ← [東京] ← BOS
```

## Module Dependencies

```
root.zig
  ├── trie.zig (DoubleArray)
  ├── dictionary.zig
  │   ├── Entry
  │   ├── ConnectionCosts
  │   └── Dictionary
  ├── lattice.zig
  │   ├── LatticeNode
  │   └── Lattice
  ├── viterbi.zig
  │   ├── Token
  │   ├── viterbi()
  │   └── buildLattice()
  ├── serialization.zig
  │   ├── serializeDictionary()
  │   └── deserializeDictionary()
  └── tokenizer.zig
      └── Tokenizer
```

## Usage Example Flow

```
Step 1: Build Dictionary
┌──────────────────────┐
│ Keywords: ["東京",   │
│           "に",      │
│           "行く"]    │
└──────────────────────┘
         │
         ▼
┌──────────────────────┐
│ Build DoubleArray    │
│ da.build()           │
└──────────────────────┘
         │
         ▼
┌──────────────────────┐
│ Create Entries       │
│ with POS info        │
└──────────────────────┘
         │
         ▼
┌──────────────────────┐
│ Set Connection Costs │
└──────────────────────┘
         │
         ▼
┌──────────────────────┐
│ Serialize to Binary  │
│ → dict.bin           │
└──────────────────────┘

Step 2: Embed Dictionary
┌──────────────────────┐
│ @embedFile(         │
│   "dict.bin"         │
│ )                    │
└──────────────────────┘
         │
         ▼
┌──────────────────────┐
│ Deserialize          │
│ at compile time      │
└──────────────────────┘

Step 3: Tokenize
┌──────────────────────┐
│ Input: "東京に行く"   │
└──────────────────────┘
         │
         ▼
┌──────────────────────┐
│ tokenizer.tokenize() │
└──────────────────────┘
         │
         ▼
┌──────────────────────┐
│ Output: [Token]      │
│  - 東京 (名詞)        │
│  - に   (助詞)        │
│  - 行く (動詞)        │
└──────────────────────┘
```

## Key Algorithms

### Double-Array Trie Search: O(m)
```
where m = length of query string

for each char in query:
  next = base[node] + char
  if check[next] != node:
    return NOT_FOUND
  node = next
return FOUND
```

### Viterbi: O(n × m²)
```
where:
  n = number of positions in text
  m = average morpheme candidates per position

Forward:
  for each position:
    for each node at position:
      for each previous node:
        cost = prev_cost + conn_cost + word_cost
        if cost < node.min_cost:
          node.min_cost = cost
          node.prev = prev_node

Backward:
  current = EOS
  while current != BOS:
    add current to path
    current = current.prev
```

## Performance Characteristics

- Dictionary Lookup: O(m) where m is query length
- Viterbi Algorithm: O(n × m²) where n is text length, m is candidates
- Memory: ~1-2MB for typical Japanese dictionary
- Binary Format: Compact, memory-mappable
