
# Layout21 

Custom integrated circuit layout.  

* A layered suite of layout-data formats, each expressed in the ProtoBuf schema description language. 
* Libraries for compiling from more abstract and terse such expressions into more-detailed. 
* Exchange with industry-standard formats such as [GDSII](./gds21) and [LEF](./lef21).

Each of the internally-defined layout21 compilers, parsers and generators are implemented in [Rust](https://www.rust-lang.org/). Cross-language compatibility of the underlying ProtoBuf-based data schema allows for usage in most other popular languages. 

Like most large Rust projects layout21 is a multi-crate workspace. Some internal crates are publicly available through [crates.io](https://crates.io). The "top-level" [layout21meta](./layout21meta) crate includes dependencies on all, and is the easiest entry-point for using all `layout21` functionality. In `layout21meta`'s namespace and in documentation most child-crates are referred to by their suffixes, i.e. `layout21::raw`. 

| Crate       | Description | [crates.io](https://crates.io) / [docs.rs](https://docs.rs) |
| ----------- | ----------- | ----------- |
| [gds21](./gds21) | GDSII Parsing, Generation, and Manipulation | ![](https://docs.rs/gds21/badge.svg) | 
| [lef21](./lef21) | LEF Parsing, Generation, and Manipulation   | |
| [layout21protos](./layout21protos) | Protobuf Schema Definitions | |
| [layout21raw](./layout21raw)       | "Raw" geometric layout, analogous to most existing layout systems. |  |
| [layout21tetris](./layout21tetris) | Gridded gate-array-style semi-custom layout | |
[layout21meta](./layout21meta) | Meta-crate including all of the above | |

