use anyhow::{Result, anyhow, bail};
use std::{
    env::{args, Args},
    path::Path,
    ffi::OsStr,
};


// Show the help menu
fn cmd_help(bin: &str) {
    let binname = Path::new(bin)
        .file_name()
        .map(OsStr::to_string_lossy)
        .unwrap_or("hulkompiler".into());
    let ver = env!("CARGO_PKG_VERSION");
    println!(r#"
Usage: {binname} -h | COMMAND [OPTIONS]

  -h, --help:
    Show this help message

Commands:
  debug-lex:
    Lex the file and dump (to stdout) the tokens

HULKompiler {ver}
"#);
}


fn cmd_dumplex(args: Args) -> Result<()> {
    todo!("Implement dump-lex")
}


fn main() -> Result<()> {

    // Parse the args
    let mut args = args();
    let bin = args.next()
        .ok_or(anyhow!("Bad args (expected binary name, found nothing)"))?;

    while let Some(arg) = args.next() {
        if arg=="-h" || arg=="--help" {
            // Show the help menu
            cmd_help(&bin);
            return Ok(());

        }else if arg=="dump-lex" {
            return cmd_dumplex(args);
        }else if arg.starts_with("--") {
            bail!("Unknown arg: {arg}, try --help");
        }else if arg.starts_with("-") {
            bail!("Unknown option: {arg}, try --help");
        }else{
            bail!("Unexpected command: {arg}, try --help");
        }
    }

    bail!("No command specified, try --help")
}
