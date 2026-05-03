# zarame

A Japanese morphological analyzer written in Zig.
It tokenizes Japanese text into words with the dictionary embedded in the binary.

## Zig version

The minimum required Zig version is 0.16.0.

## UniDic version

Only unidic-cwj-202512 is supported at this time.

## Usage

### Running the sample application

Run the following command to launch the sample application.
It tokenizes Japanese text read from standard input into individual words.

```sh
zig build run -Doptimize=ReleaseSafe
```

> ```sh
> $ zig build run -Doptimize=ReleaseSafe
> 今日はいい天気ですね。
> Loaded 1027505 morphs, matrix rows=24082, cols=21375. Double array size is 2688901.
> BOS
> 今日
> は
> いい
> 天気
> です
> ね
> 。
> EOS
> ```

Note: the application uses several gigabytes of memory at runtime.

### Building the dictionary

Place `lex.csv` and `matrix.def` in `src/resource/`, then run:

```sh
zig build dictionary -Doptimize=ReleaseSafe
```

Note: the build process uses several gigabytes of memory.
Once complete, `src/zarame.dict.gz` will be generated.
