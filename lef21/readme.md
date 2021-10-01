# lef21


## Lef21 Library Exchange Format (LEF) Parser & Writer

[Library Exchange Format (LEF)](https://en.wikipedia.org/wiki/Library_Exchange_Format)
is an ASCII text based format for integrated circuit (IC) layout and technology.

LEF is near-ubiquitously used IC-industry-wide for two related purposes:

* *Libraries* of LEF *Macros* commonly provide the *physical abstract* view of a circuit design.
  * Such abstract-views are commonly the target for layout-synthesis programs ("place and route").
  * They include a circuit's pin locations and requirements for "obstruction" blockages, among other metadata, typically without including the circuit's internal layout implementation.
* LEF *technology descriptions* ("tech-lef") provide a concise description of rules for assembling such cells, as commonly performed by layout-synthesis software.

Lef21 includes comprehensive support for parsing and writing LEF *design libraries*, primarily stored as its [LefLibrary] type.
A select subset of tech-lef features are also supported, particularly those which blur the lines between technology and library data.

### Usage

Creating a [`LefLibrary`] from file solely requires a call to the [LefLibrary::open] method:

```skip
use lef21::LefLibrary;
let lib = LefLibrary::open("mylib.lef")?;
```

Each [`LefLibrary`] is a short tree of macro-definitions, which are in turn primarily comprised of pin-definitions and obstructions.
The shape of this tree is of the form:

* [`LefLibrary`]
  * Library Metadata
  * Vec<[`LefMacro`]>
    * Macro Metadata
    * Vec<[`LefPin`]>
      * Pin Metadata
      * Vec<[`LefPort`]>
    * Obstructions ([`LefLayerGeometries`])

All fields of all layers in the [`LefLibrary`] tree are publicly accessible and modifiable.

### Serialization

[`LefLibrary`], all underlying data structures, and all Lef21's other primary data stores are [`serde`](https://crates.io/crates/serde) serializable,
and can be straightforwardly converted to and from any serde-compatible format. Examples:

```skip
let lib = lef21::LefLibrary::new();
let json = serde_json::to_string(&lib);
let yaml = serde_yaml::to_string(&lib);
let toml = toml::to_string(&lib);
```

Lef21 includes built-in support for a subset of serde-formats via its [`SerializationFormat`] enumeration,
and support for directly reading and writing files in each format via its accompanying [`SerdeFile`] trait.
Example using [`SerializationFormat::Yaml`]:

```skip
use lef21::SerializationFormat::Yaml;
let lib = lef21::LefLibrary::new();

// Write to YAML-format file
Yaml.save(&lib, "mylib.lef.yaml")?;
// And read back from file
let lib2: lef21::LefLibrary = Yaml.open("mylib.lef.yaml")?;
```

### Background

Lef21 is a subset of the larger [Layout21](https://github.com/dan-fritchman/Layout21) library, and is primarily used as an import and export layer.
Lef21 correspondingly uses the LEF format's concepts, idioms, and terminology (e.g. "macro" vs. "cell") throughout.
Its LEF data structures are nonetheless designed for direct manipulation, for example in programmatically modifying existing LEF content.

LEF is frequently paired with the DEF format for specifying circuit's internal physical implementations.
Like LEF, DEF is a text-based format. More common industry usage pairs LEF with [GDSII](https://crates.io/crates/gds21)'s binary implementation format,
which dramatically reduces data-sizes for large circuits.
DEF is not supported by Lef21. GDSII is supported by the related [gds21](https://crates.io/crates/gds21) crate.

LEF was originally designed by Tangent Systems, later acquired by Cadence Design Systems.
Lef21 holds no relationship to either entity, nor any authority or ownership over the format.
Countless LEF-format design descriptions are freely available as open-source software;
their examples serve as the basis for Lef21.

License: BSD-3-Clause
