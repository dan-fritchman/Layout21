use std::error::Error;
use std::env;
use std::process;
use lef21::LefLibrary;

struct Config {
    inlef: String,
    outlef: String
}

impl Config {
    fn new(args: &[String]) -> Result<Config, &'static str> {    
        if args.len() < 3 {
            return Err("Not enough arguments, expecting 2.")
        }
        let inlef = args[1].clone();
        let outlef = args[2].clone();
        Ok(Config { inlef, outlef} )
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let args : Vec<String> = env::args().collect();
    let cfg = Config::new(&args)?;
    let lib = LefLibrary::open(cfg.inlef)?;
    lib.save(cfg.outlef)?;
    Ok(())
}
fn main() {
    run().unwrap_or_else(|err| {
        println!("Problem in lefrw: {}", err);
        process::exit(1);
    });
}