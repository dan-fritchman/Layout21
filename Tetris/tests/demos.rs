// Local imports
use super::{exports, stacks::SampleStacks};
use crate::{
    conv::proto::ProtoLibImporter, protos, raw::LayoutResult, utils::SerializationFormat::Yaml,
};

#[test]
fn empty() -> LayoutResult<()> {
    behind_curtain(include_str!("empty.yaml"))
}

#[test]
fn insts() -> LayoutResult<()> {
    behind_curtain(include_str!("insts.yaml"))
}

fn behind_curtain(yaml: &str) -> LayoutResult<()> {
    let plib: protos::tetris::Library = Yaml.from_str(yaml)?;
    let lib = ProtoLibImporter::import(&plib)?;
    exports(lib, SampleStacks::pdka()?)
}
