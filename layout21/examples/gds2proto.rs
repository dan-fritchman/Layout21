use layout21protos::ProtoFile;
use layout21raw::gds::GdsImporter;
use layout21raw::proto::ProtoExporter;
use layout21raw::LayoutError;
use structopt::StructOpt;

#[derive(StructOpt)]
struct ProgramOptions {
    #[structopt(short = "i", long, default_value = "")]
    gds: String,
    #[structopt(short = "o", long, default_value = "")]
    proto: String,
    #[structopt(short, long)]
    verbose: bool,
}

fn main() -> Result<(), LayoutError> {
    let options = ProgramOptions::from_args();

    let lib = gds21::GdsLibrary::load(&options.gds).unwrap();

    if options.verbose {
        println!("read: {:?}", options.gds);
    }

    if options.verbose {
        let gds_stats = lib.stats();
        println!("{:?}", gds_stats);
    }

    let lib = GdsImporter::import(&lib, None)?;

    let proto_lib = ProtoExporter::export(&lib)?;
    proto_lib.save(&options.proto).unwrap();

    if options.verbose {
        println!("wrote: {:?}", &options.proto);
    }

    Ok(())
}
