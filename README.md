# benc

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

| Library     | `crossref`       | `ubuntu`         |
| ----------- | ---------------- | ---------------- |
| benc        | 27.6 ms ± 2.1 ms | 1.46 μs ± 93 ns  |
| bencode     | 218 ms ± 11 ms   | 28.4 μs ± 1.5 μs |
| AttoBencode | 44.8 ms ± 3.8 ms | 2.97 μs ± 171 ns |
| bencoding   | 39.7 ms ± 3.7 ms | 2.38 μs ± 181 ns |

<sup>Note: `bencode` parses from a lazy `ByteString` unlike the rest which parse
from strict `ByteString`s, and so is expected to be a little slower.</sup>

#### Encoding

| Library     | `crossref`       | `ubuntu`         |
| ----------- | ---------------- | ---------------- |
| benc        | 11.8 ms ± 1.0 ms | 1.91 μs ± 179 ns |
| bencode     | 42.4 ms ± 3.1 ms | 3.19 μs ± 173 ns |
| AttoBencode | 20.2 ms ± 1.1 ms | 10.2 μs ± 387 ns |
| bencoding   | 11.6 ms ± 1.1 ms | 1.79 μs ± 100 ns |

<sup>Note: `AttoBencode` encodes to a strict `ByteString` via a lazy
`ByteString`, unlike the rest, which only prepare the lazy `ByteString`. As
such, it is expected to be slower.</sup>

## Contributing

Contributions, bug reports, suggestions welcome! Please
[open an issue](https://github.com/meooow25/benc/issues).
