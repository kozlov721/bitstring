# bitstring

A fast and simple Haskell library for bit-level manipulation, built as a wrapper
around `Data.ByteString.Lazy`. It enables construction and deconstruction of
byte strings at the granularity of individual bits — useful for binary parsing,
encoding, and low-level data manipulation tasks.

This is an updated version of [Data.BitString](https://hackage.haskell.org/package/bitstring-0.0.0).

📖 **[API Documentation](https://kozlov721.github.io/bitstring/)**

---

## Features

- **Bit-level construction** — build `BitString`s one bit at a time using `cons` / `snoc`
- **Integral conversion** — convert to and from any `Integral` type with optional zero-padding
- **Two endianness variants**:
  - `Data.BitString` — little-endian bit ordering (default)
  - `Data.BitString.BigEndian` — big-endian bit ordering
- **Rich API** mirroring standard list/`ByteString` operations (`take`, `drop`, `reverse`, `zip`, `fold`, …)
- **Type class instances** — `Monoid`, `Bits`, `Ord`, `IsList`, `Show`, `Read`
- **File I/O** — `readFile`, `writeFile`, `appendFile`
- **Pattern synonyms** — `Empty` and `(:::)` for ergonomic pattern matching

---

## Installation

### Cabal

Add `bitstring` to the `build-depends` field in your `.cabal` file:

```cabal
build-depends:
    base >= 4.7 && < 5
  , bitstring
```

### Stack

Add `bitstring` to your `package.yaml` dependencies:

```yaml
dependencies:
  - base >= 4.7 && < 5
  - bitstring
```

---

## Quick Start

```haskell
import qualified Data.BitString as BS

-- Construct a BitString from a list of bits (0s and 1s)
let bs = BS.pack [1, 0, 1, 1, 0, 0, 1, 0]

-- Convert to/from numbers
let n  = BS.toNumber bs :: Int    -- 178
let bs2 = BS.fromNumber (42 :: Int)

-- Bit-level cons / pattern matching
let bs3 = 1 `BS.cons` 0 `BS.cons` BS.empty

case bs3 of
  BS.Empty    -> putStrLn "empty"
  b BS.::: rest -> print b        -- prints 1

-- Take / drop
let first4 = BS.take 4 bs        -- [1,0,1,1]
let last4  = BS.takeEnd 4 bs     -- [0,0,1,0]

-- Conversion to/from ByteString
import qualified Data.ByteString.Lazy as BL
let bytes = BS.toByteString bs
let bs4   = BS.fromByteString bytes

-- Bitwise operations (Bits instance)
let x = BS.pack [1,0,1,0]
let y = BS.pack [1,1,0,0]
let z = x .&. y   -- [1,0,0,0]
let w = x .|. y   -- [1,1,1,0]

-- Big-endian variant
import qualified Data.BitString.BigEndian as BSE
let be = BSE.fromNumber (42 :: Int)
```

---

## Modules

| Module | Description |
|---|---|
| `Data.BitString` | Default module — bits stored in little-endian order within each byte |
| `Data.BitString.BigEndian` | Same API — bits stored in big-endian order within each byte |

Both modules are intended to be imported `qualified` to avoid clashes with `Prelude`.

---

## API Overview

### Types

| Name | Description |
|---|---|
| `BitString` | Core type — wrapper around a lazy `ByteString` |
| `Bit` | Alias for `Word8`; only `0` and `1` are valid values |

### Construction

| Function | Complexity | Description |
|---|---|---|
| `empty` | O(1) | Empty `BitString` |
| `singleton` | O(1) | Single-bit `BitString` |
| `pack` | O(n) | From a list of `Bit`s |
| `packB` | O(n) | From a list of `Bool`s |
| `fromByteString` | O(1) | From a lazy `ByteString` |
| `fromByteStringStrict` | O(1) | From a strict `ByteString` |
| `fromNumber` | O(log n) | From an `Integral` value |
| `fromNumberPadded` | O(log n) | From a fixed-size integral with leading zeros |
| `replicate` | O(n) | Repeat a bit value n times |
| `cons` / `(:::)` | O(1) | Prepend a bit |
| `snoc` | O(n) | Append a bit |

### Deconstruction

| Function | Complexity | Description |
|---|---|---|
| `unpack` | O(n) | To a list of `Bit`s |
| `unpackB` | O(n) | To a list of `Bool`s |
| `toByteString` | O(1) | To a lazy `ByteString` (zero-padded) |
| `toByteStringStrict` | O(1) | To a strict `ByteString` |
| `toByteStringPadded` | O(1) | To `ByteString` plus padding count |
| `toNumber` | O(n) | To an `Integral` value |
| `head` / `headB` | O(1) | First bit |
| `tail` | O(1) | Everything after the first bit |
| `last` / `lastB` | O(n) | Last bit |
| `init` | O(n) | Everything before the last bit |
| `uncons` / `unconsB` | O(1) | Split head from tail |
| `unsnoc` / `unsnocB` | O(n) | Split init from last |

### Transformations

| Function | Complexity | Description |
|---|---|---|
| `reverse` | O(n) | Reverse all bits |
| `take` | O(n) | Prefix of length n |
| `drop` | O(n) | Suffix after dropping n bits |
| `takeEnd` | O(n) | Suffix of length n |
| `dropEnd` | O(n) | Prefix after dropping n from end |
| `splitAt` | O(n) | Split at position |
| `splitAtEnd` | O(n) | Split from the end |
| `stripZeros` | O(c) | Strip leading zero bits |
| `stripOnes` | O(c) | Strip leading one bits |
| `stripZerosEnd` | O(n) | Strip trailing zero bits |
| `stripOnesEnd` | O(n) | Strip trailing one bits |
| `append` / `(<>)` | O(n) | Concatenate two `BitString`s |
| `concat` | O(n·m) | Concatenate a list of `BitString`s |
| `paddEqual` | O(1) | Zero-pad the shorter of two `BitString`s |

### Folding & Zipping

| Function | Description |
|---|---|
| `foldr` / `foldr'` | Right fold over bits |
| `foldl` / `foldl'` | Left fold over bits |
| `zip` / `zipB` | Zip two `BitString`s into a list of pairs |
| `zipWith` | Zip with a combining function |
| `packZipWith` | Zip and re-pack the result into a `BitString` |

### Indexing & Search

| Function | Complexity | Description |
|---|---|---|
| `(!)` | O(n) | Bit at index (unsafe) |
| `(!?)` | O(n) | Bit at index (safe) |
| `findSubstring` | O(n·m) | Find first occurrence of a sub-`BitString` |

### File I/O

| Function | Description |
|---|---|
| `readFile` | Read a file into a `BitString` |
| `writeFile` | Write a `BitString` to a file |
| `appendFile` | Append a `BitString` to a file |

---

## License

BSD 3-Clause — see [LICENSE](LICENSE).
