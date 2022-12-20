
# Layout21 Protos Crate 

A light wrapper around the [Vlsir](https://crates.io/crates/vlsir) Rust bindings. 

## Using Pre-Release Vlsir

To use pre-released git-based versions of `vlsir`, 
clone/put the Vlsir repo to a folder named `vlsir` in this directory, then run
the build script to generate the rust bindings:

```
git clone git@github.com:vlsir/vlsir
cd vlsir
scripts/build.sh
cd -
cargo test
```
