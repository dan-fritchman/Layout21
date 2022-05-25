
# Layout21 

This repository is copied from [dan-fritchman/Layout21](https://github.com/dan-fritchman/Layout21).

Custom integrated circuit layout.  

[![test](https://github.com/dan-fritchman/Layout21/actions/workflows/test.yml/badge.svg)](https://github.com/dan-fritchman/Layout21/actions/workflows/test.yml)

* A layered suite of layout-data formats, each expressed in the ProtoBuf schema description language. 
* Libraries for compiling from more abstract and terse such expressions into more-detailed. 
* Exchange with industry-standard formats such as [GDSII](./gds21) and [LEF](./lef21).

Each of the internally-defined `layout21` compilers, parsers and generators are implemented in [Rust](https://www.rust-lang.org/). Cross-language compatibility of the underlying [ProtoBuf](https://developers.google.com/protocol-buffers)-based data schema allows for usage in most other popular languages. 

Like most large Rust projects `layout21` is a multi-crate workspace. Some internal crates are publicly available through [crates.io](https://crates.io). The "top-level" [layout21](./layout21) crate includes dependencies on all, and is the easiest entry-point for using all `layout21` functionality. In `layout21`'s namespace and in documentation most child-crates are referred to by their suffixes, i.e. `layout21::raw`. 

| Crate       | Description | [crates.io](https://crates.io) | [docs.rs](https://docs.rs) |
| ----------- | ----------- | ------------------------------ | -------------------------- |
| [gds21](./gds21) | GDSII Parsing, Generation, and Manipulation | [![](https://img.shields.io/crates/v/gds21.svg)](https://crates.io/crates/gds21) | [![](https://docs.rs/gds21/badge.svg)](https://docs.rs/gds21) | 
| [lef21](./lef21) | LEF Parsing, Generation, and Manipulation   | [![](https://img.shields.io/crates/v/lef21.svg)](https://crates.io/crates/lef21) | [![](https://docs.rs/lef21/badge.svg)](https://docs.rs/lef21) | 
| [layout21protos](./layout21protos) | Protobuf Schema Definitions | | | 
| [layout21raw](./layout21raw)       | "Raw" geometric layout. Analogous to most existing layout systems. |  | | 
| [layout21tetris](./layout21tetris) | Gridded gate-array-style semi-custom layout | | | 
| [layout21](./layout21) | Meta-crate including all of the above | | | 

