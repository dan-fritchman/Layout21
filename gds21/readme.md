
# Gds21

[![docs](https://docs.rs/gds21/badge.svg)](https://docs.rs/gds21)


## Gds21 Integrated Circuit Layout Parser & Writer

GDSII is the IC industry's de facto standard for storing and sharing layout data.
Gds21 is a library for reading and creating GDSII data, similar to and largely inspired by libraries such as [gdstk](https://github.com/heitzmann/gdstk) and its predecessor [gdspy](https://github.com/heitzmann/gdspy).
Gds21 differs in being designed primarily as an interface layer to GDSII for the larger [Layout21](https://github.com/dan-fritchman/Layout21) library.
Reading and generating GDSII-format data are primary goals;
offering ease-of-use functionality for more elaborate manipulations of GDS data is not.
(Although these manipulations can be performed on Gds21's data structures).
Gds21 accordingly stores layout data on GDSII's terms, using GDSII's idioms, naming conventions, and datatypes.

Layout data is represented in three primary forms:

* A short tree with three layers:
  * The root is a [`GdsLibrary`], which primarily consists of a set of cells ([`GdsStruct`]s), and secondarily a set of metadata.
    Each [`GdsLibrary`] is a universe unto itself, in that it has no mechanisms for comprehending layout cells or data defined outside itself.
    On-disk each [`GdsLibrary`] is typically paired one-to-one with a `.gds` file.
  * Libraries consist of cell definitions AKA [`GdsStruct`]s, which define each layout cell (or module, or "struct" in GDSII terms).
  * Cells consist of [`GdsElement`]s, an enumeration which includes individual polygons ([`GdsBoundary`]),
    instances of other layout cells ([`GdsStructRef`]), text ([`GdsTextElem`]), and a few other geometric elements.
* For storage on disk, the [`GdsLibrary`] tree is flattened to a series of [`GdsRecord`]s.
  These records indicate the beginning, end, and content of each tree-node.
  Detailed descriptions of these records comprise the majority of the GDSII spec.
* Records are stored on-disk in binary form as detailed in the GDSII spec.
  Each includes a record-type header, datatype, length field, and optional additional content.
  These raw-bytes are never stored by Gds21, only generated and consumed on their way into and out of [`Read`] and [`Write`] objects (typically [`File`]s).


### Usage

Loading a [`GdsLibrary`] from disk:

```skip
let lib = GdsLibrary::load("sample.gds")?;
```

Creating a new and empty [`GdsLibrary`], and adding a [`GdsStruct`] cell-definition:

```rust
use gds21::{GdsLibrary, GdsStruct};
let mut lib = GdsLibrary::new("mylib");
lib.structs.push(GdsStruct::new("mycell"));
```

Saving a [`GdsLibrary`] to disk:

```skip
lib.save("mylib.gds");
```

### Serialization

Each element in Gds21's [`GdsLibrary`] tree is [`serde`]-serializable.
GDSII data can be straightforwardly serialized in any serde-supported format.
Examples:

```
let lib = gds21::GdsLibrary::new("mylib");
let json = serde_json::to_string(&lib);
let yaml = serde_yaml::to_string(&lib);
let toml = toml::to_string(&lib);
```

Gds21 includes built-in support for a subset of serde-formats via its [`SerializationFormat`] enumeration,
and support for directly reading and writing files in each format via its accompanying [`SerdeFile`] trait.
Example using [`SerializationFormat::Yaml`]:

```skip
use gds21::SerializationFormat::Yaml;
let lib = gds21::GdsLibrary::new("mylib");

// Write to YAML-format file
Yaml.save(&lib, "mylib.gds.yaml")?;
// And read back from file
let lib2: gds21::GdsLibrary = Yaml.open("mylib.gds.yaml")?;
```

Note these text-based representations will generally be substantially larger than binary GDSII data.

