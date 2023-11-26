# benc

[![Hackage](https://img.shields.io/hackage/v/benc?logo=haskell&color=blue)](https://hackage.haskell.org/package/benc)
[![Haskell-CI](https://github.com/meooow25/benc/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/meooow25/benc/actions/workflows/haskell-ci.yml)

Bencode encoding and decoding library

## Bencode

Bencode is a simple encoding format for loosely structured data, comparable to
JSON. It is used primarily in the BitTorrent protocol. For a description of the
format see

* [The BitTorrent Protocol Specification (BEP-3)](https://www.bittorrent.org/beps/bep_0003.html)
* [Bencode on Wikipedia](https://en.wikipedia.org/wiki/Bencode)

## Features

This library offers

* A nice API
* Correctness
* Speed

This library does not attempt to support

* Lazy or incremental parsing
* Failing with detailed error messages

## Getting started

Please see the Haddocks for [`Data.Bencode.Decode`](https://hackage.haskell.org/package/benc/docs/Data-Bencode-Decode.html)
and [`Data.Bencode.Encode`](https://hackage.haskell.org/package/benc/docs/Data-Bencode-Encode.html).

## Alternatives

There are currently three other Bencode libraries on Hackage:

* [bencode](https://hackage.haskell.org/package/bencode)
* [AttoBencode](https://hackage.haskell.org/package/AttoBencode)
* [bencoding](https://hackage.haskell.org/package/bencoding)

All of these are in some combination of buggy, slow, and unmaintained.

<details>
<summary>Click for details</summary>

* `bencode`:
  * Bugs (e.g. crashes on input `"i-e"`)
  * Very slow parsing
  * No high-level encoding API
  * [Minor] Lax parsing (e.g. admits the invalid `"i-0e"`)
* `AttoBencode`
  * Slow parsing
  * [Minor] Lax parsing (e.g. admits the invalid `"i-0e"`)
* `bencoding`
  * Bugs (e.g. crashes on parsing non-UTF-8 into Text)
  * Questionable design of dict encoding/decoding API, where human error can
    lead to mis-parsing Bencode or writing invalid Bencode.
  * [Minor] Lax parsing (e.g. admits the invalid `"i-0e"`)

</details>

### API comparison

See the [benchmark file](https://github.com/meooow25/benc/blob/master/compare/Bench.hs)
as a comparison point of the library APIs.

### Benchmarks

Below is a comparison of decoding and encoding of two torrent files, performed
with GHC 9.6.3. See the [benchmark file](https://github.com/meooow25/benc/blob/master/compare/Bench.hs)
for details.

#### Decoding

| Library     | `crossref` time  | alloc  | `ubuntu` time    | alloc  |
| ----------- | ---------------- | ------ | ---------------- | ------ |
| benc        | 21.3 ms ± 902 μs | 24 MB  | 1.30 μs ± 90 ns  | 3.6 KB |
| bencode     | 218 ms ± 7.7 ms  | 737 MB | 29.0 μs ± 2.6 μs | 121 KB |
| AttoBencode | 44.6 ms ± 4.0 ms | 129 MB | 3.01 μs ± 102 ns | 17 KB  |
| bencoding   | 39.1 ms ± 2.3 ms | 104 MB | 2.44 μs ± 175 ns | 15 KB  |

<sup>Note: `bencode` parses from a lazy `ByteString` unlike the rest which parse
from strict `ByteString`s, and so is expected to be a little slower.</sup>

#### Encoding

| Library     | `crossref` time  | alloc  | `ubuntu` time    | alloc  |
| ----------- | ---------------- | ------ | ---------------- | ------ |
| benc        | 9.17 ms ± 487 μs | 42 MB  | 1.58 μs ± 101 ns | 11 KB  |
| bencode     | 37.8 ms ± 860 μs | 113 MB | 3.17 μs ± 174 ns | 19 KB  |
| AttoBencode | 19.7 ms ± 1.8 ms | 109 MB | 10.1 μs ± 993 ns | 295 KB |
| bencoding   | 11.9 ms ± 916 μs | 67 MB  | 1.81 μs ± 129 ns | 15 KB  |

<sup>Note: `AttoBencode` encodes to a strict `ByteString` via a lazy
`ByteString`, unlike the rest, which only prepare the lazy `ByteString`. As
such, it is expected to be slower.</sup>

## Contributing

Contributions, bug reports, suggestions welcome! Please
[open an issue](https://github.com/meooow25/benc/issues).
