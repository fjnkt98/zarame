# Zarame - Japanese Morphological Analyzer in Zig

Zarame is a Japanese morphological analyzer (形態素解析器) implemented in Zig. It provides efficient tokenization of Japanese text using the Viterbi algorithm and double-array trie data structure.

## Features

- **Double-Array Trie**: Efficient dictionary lookup using double-array trie implementation
- **Viterbi Algorithm**: Finds the optimal morpheme sequence for Japanese text
- **Binary Dictionary Format**: Compact binary format for dictionary data that can be embedded in the executable
- **MeCab-Compatible**: Dictionary format compatible with MeCab-style dictionaries

## Architecture

### Core Components

1. **Trie (Double-Array)** (`src/trie.zig`)
   - Efficient prefix matching for dictionary lookup
   - Space-efficient double-array implementation
   - Supports common prefix search and exact matching

2. **Dictionary** (`src/dictionary.zig`)
   - Entry structure: surface form, context IDs, word cost, POS features
   - Connection cost matrix for morpheme transitions
   - Dictionary lookup interface

3. **Lattice** (`src/lattice.zig`)
   - Represents all possible morpheme sequences for input text
   - Nodes contain morpheme candidates with costs
   - BOS (Beginning of Sentence) and EOS (End of Sentence) markers

4. **Viterbi Algorithm** (`src/viterbi.zig`)
   - Finds optimal path through the lattice
   - Forward pass: calculate minimum cumulative costs
   - Backward pass: backtrack to find best path
   - Returns sequence of tokens

5. **Tokenizer** (`src/tokenizer.zig`)
   - Main interface for text tokenization
   - Handles unknown words
   - Character type classification (Hiragana, Katakana, Kanji, etc.)

6. **Serialization** (`src/serialization.zig`)
   - Binary format for dictionary storage
   - Serialize/deserialize double-array, entries, and connection costs
   - Can be used with `@embedFile` to embed dictionaries in the binary

## Building a Dictionary

### Dictionary File Format

The dictionary requires three components:

1. **Dictionary Entries (CSV format)**:
```csv
surface,left_id,right_id,cost,pos1,pos2,pos3,...
東京,1,1,3000,名詞,固有名詞,地名,一般
に,2,2,500,助詞,格助詞,一般,*
```

2. **Connection Costs Matrix**:
- Matrix of connection costs between morphemes
- Size: `forward_size × backward_size`
- Format: binary or CSV

3. **Character Definitions (for unknown words)**:
- Define character types and their costs
- Used when dictionary lookup fails

### Building and Serializing Dictionary

```zig
const std = @import("std");
const zarame = @import("zarame");

pub fn buildDictionary(allocator: std.mem.Allocator) !void {
    // 1. Build double-array trie from surface forms
    const keywords = [_][]const u8{
        "東京",
        "に",
        "行く",
        // ... more entries
    };
    
    var da = try zarame.DoubleArray.init(allocator, &keywords);
    defer da.deinit();
    try da.build();
    
    // 2. Create dictionary entries
    var entries = std.ArrayList(zarame.Entry).init(allocator);
    defer entries.deinit();
    
    const features = [_][]const u8{ "名詞", "固有名詞", "地名" };
    const entry = zarame.Entry.init("東京", 1, 1, 3000, &features);
    try entries.append(entry);
    
    // 3. Create connection costs
    var costs = try zarame.ConnectionCosts.init(allocator, 100, 100);
    defer costs.deinit();
    costs.setCost(1, 2, 100); // Set connection costs
    
    // 4. Serialize to binary file
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

## Embedding Dictionary in Binary (like Kagome)

To embed the dictionary in the executable binary like Kagome:

1. **Build the binary dictionary**:
```bash
# Create your dictionary builder tool
zig build-exe build_dict.zig
./build_dict
# This creates dict.bin
```

2. **Embed in your application**:
```zig
const dict_data = @embedFile("dict.bin");

pub fn loadDictionary(allocator: std.mem.Allocator) !zarame.Dictionary {
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

## Usage Example

```zig
const std = @import("std");
const zarame = @import("zarame");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // Load dictionary (from embedded binary or file)
    var dictionary = try loadDictionary(allocator);
    defer dictionary.deinit();
    
    // Create tokenizer
    var tokenizer = zarame.Tokenizer.init(allocator, dictionary);
    defer tokenizer.deinit();
    
    // Tokenize text
    const text = "東京に行く";
    const tokens = try tokenizer.tokenize(text);
    defer allocator.free(tokens);
    
    // Print results
    for (tokens) |token| {
        std.debug.print("{s}\t", .{token.surface});
        for (token.features) |feature| {
            std.debug.print("{s},", .{feature});
        }
        std.debug.print("\n", .{});
    }
}
```

## Implementation Steps

### ✅ Phase 1: Core Data Structures (Complete)
- ✅ Double-array trie implementation
- ✅ Dictionary entry structure
- ✅ Connection cost matrix
- ✅ Lattice structure for Viterbi

### ✅ Phase 2: Algorithms (Complete)
- ✅ Viterbi algorithm implementation
- ✅ Lattice building from dictionary
- ✅ Token extraction from optimal path

### ✅ Phase 3: Binary Format (Complete)
- ✅ Serialization for double-array
- ✅ Serialization for dictionary entries
- ✅ Serialization for connection costs
- ✅ Complete dictionary serialization/deserialization

### ✅ Phase 4: Tokenizer Interface (Complete)
- ✅ Main tokenizer class
- ✅ Character type classification
- ✅ Integration with all components

### 🔄 Phase 5: Next Steps (To Do)
- ⬜ Dictionary builder tool from CSV
- ⬜ Unknown word handling implementation
- ⬜ Performance optimization
- ⬜ Comprehensive testing with real dictionaries
- ⬜ Documentation and examples

## Building and Testing

```bash
# Build the library
zig build

# Run tests
zig build test

# Build and run the example
zig build run
```

## Technical Details

### Dictionary Binary Format

The binary format consists of:

```
Header (fixed size):
- Magic number (4 bytes): 0x4D524157
- Version (2 bytes): 1
- Entry count (4 bytes)
- DA base/check sizes (4 bytes each)
- Connection cost dimensions (2 bytes each)
- Section offsets (8 bytes each)

Double-Array Section:
- Base array size (4 bytes)
- Base array data (i32 × size)
- Check array size (4 bytes)
- Check array data (i32 × size)

Entries Section:
- Entry count (4 bytes)
- For each entry:
  - Surface length (2 bytes)
  - Surface data (UTF-8)
  - Left ID (2 bytes)
  - Right ID (2 bytes)
  - Cost (2 bytes)
  - Feature count (2 bytes)
  - For each feature:
    - Length (2 bytes)
    - Data (UTF-8)

Connection Costs Section:
- Forward size (2 bytes)
- Backward size (2 bytes)
- Cost matrix (i16 × forward × backward)
```

### Viterbi Algorithm

The implementation uses dynamic programming:

1. **Forward Pass**: Calculate minimum cumulative cost for each node
   - `cost[node] = min(cost[prev] + connection_cost + word_cost)`

2. **Backward Pass**: Backtrack from EOS to BOS to find optimal path
   - Follow the `prev_node` pointers from minimum cost node

3. **Path to Tokens**: Convert node sequence to token sequence
   - Extract surface forms and features for each node

## Performance Considerations

- **Memory**: Double-array is space-efficient but requires careful capacity management
- **Speed**: Prefix search is O(m) where m is the length of the query string
- **Dictionary Size**: Binary format is compact and can be memory-mapped
- **Unknown Words**: Character-type based fallback for OOV (out-of-vocabulary) words

## License

See LICENSE file for details.

## References

- [MeCab](https://taku910.github.io/mecab/) - Japanese morphological analyzer
- [Kagome](https://github.com/ikawaha/kagome) - Pure Go morphological analyzer
- Double-Array Trie: Aoe, J. (1989). "An Efficient Digital Search Algorithm by Using a Double-Array Structure"
- Viterbi Algorithm: Viterbi, A. J. (1967). "Error bounds for convolutional codes and an asymptotically optimum decoding algorithm"
