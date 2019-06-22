# nicer-mecab

This provides a nicer interface over
[mecab](http://taku910.github.io/mecab/), via the rust
bindings provided [here](https://crates.io/crates/mecab).

Mecab is a Japanese morphological analyzer, and it can be
used to parse a sentence into its corresponding parts,
and provide information about those parts.

Mecab provides a dump of that information as a big string,
and not as a data structure we can easily use. This library
wraps Mecab's output to make it easier to consume in programs.

## disclaimer

I am neither a linguist, nor an expert of Japanese grammer:
terminology used in this crate is likely to not be standard.
If you know more about the correct terms for specific grammar,
feel free to open an issue about it.
