use anyhow::{anyhow, bail, Result};
use std::{
    env::{args, Args},
    ffi::OsStr,
    path::Path,
};

/// Show the help menu
fn cmd_help(binname: &str) {
    let ver = env!("CARGO_PKG_VERSION");
    println!(
        r#"
Usage: {binname} -h | COMMAND [OPTIONS]

Options:

  -h, --help:
    Show this help message

Commands:
  emit-c:
    Emit the HIR (high level intermediate representation) as C code
  dump-lex:
    (DEBUG) Lex the file and dump (to stdout) the tokens
  dump-ast:
    (DEBUG)Parse the file and dump (to stdout) the AST

HULKompiler {ver}
"#
    );
}

/// Print the help message of the dump-lex command
fn cmd_dumplex_help(binname: &str) {
    let ver = env!("CARGO_PKG_VERSION");
    println!(
        r#"
Usage: {binname} dump-lex [OPTIONS] FILE

Dump the lexed data (to debug purposes)

Options:

  -h, --help:
    Show this help message
  -w, --wide
    Print the data prittier

HULKompiler {ver}
"#
    );
}

/// Dump the lex result
fn cmd_dumplex(binname: &str, mut args: Args) -> Result<()> {
    let mut file = None;
    let mut wide = false;

    // Parse the remaining args
    while let Some(arg) = args.next() {
        if arg == "-h" || arg == "--help" {
            cmd_dumplex_help(binname);
            return Ok(());
        } else if arg == "-w" || arg == "--wide" {
            wide = true;
        } else if arg.starts_with("--") {
            bail!("Unknown arg: {arg}");
        } else if arg.starts_with("-") {
            bail!("Unknown option: {arg}");
        } else if file.is_none() {
            file = Some(arg);
        } else {
            bail!("Unexpected value: {arg}");
        }
    }

    let Some(file) = file else {
        bail!("No file specified");
    };

    // Read the contents
    let data = std::fs::read_to_string(file)?;

    // Lex the contents
    let res = hulkompiler::lex::tokenize_data(&data)?
        .into_iter()
        .map(|(_, tk, slic)| (tk, slic))
        .collect::<Vec<_>>();
    println!("Result:");
    if wide {
        println!("{res:#?}");
    } else {
        println!("{res:?}");
    }

    Ok(())
}

/// Help about dump-ast
fn cmd_dumpast_help(binname: &str) {
    let ver = env!("CARGO_PKG_VERSION");
    println!(
        r#"
Usage: {binname} dump-ast [OPTIONS] FILE

Dump the parsed data (to debug purposes)

Options:

  -h, --help:
    Show this help message
  -w, --wide
    Print the data prettier

HULKompiler {ver}
"#
    );
}

/// Dump to stdout the abstract syntax tree
fn cmd_dumpast(binname: &str, mut args: Args) -> Result<()> {
    let mut file = None;
    let mut wide = false;

    while let Some(arg) = args.next() {
        if arg == "-h" || arg == "--help" {
            // Print help and exit
            cmd_dumpast_help(binname);
            return Ok(());
        } else if arg == "-w" || arg == "--wide" {
            wide = true;
        } else if arg.starts_with("--") {
            bail!("Unknown arg: {arg}");
        } else if arg.starts_with("-") {
            bail!("Unknown option: {arg}");
        } else if file.is_none() {
            file = Some(arg);
        } else {
            bail!("Unexpected value: {arg}");
        }
    }

    let Some(file) = file else {
        bail!("No file specified");
    };

    // Read the file
    let content = std::fs::read_to_string(file)?;

    // Parse
    let res = hulkompiler::ast::Parser::parse(&content)?;

    println!("Result:");
    if wide {
        println!("{res:#?}");
    } else {
        println!("{res:?}");
    }

    Ok(())
}

/// Help about the emit-c command
fn cmd_emitc_help(binname: &str) {
    let ver = env!("CARGO_PKG_VERSION");
    println!(
        r#"
Usage: {binname} emit-c [OPTIONS] FILE

Write the HIR (high level intermediate representation) as C code

Options:

  -h, --help:
    Show this help message

HULKompiler {ver}
"#
    );
}

fn cmd_emitc(binname: &str, mut args: Args) -> Result<()> {
    let mut file = None;

    while let Some(arg) = args.next() {
        if arg == "-h" || arg == "--help" {
            cmd_emitc_help(binname);
            return Ok(());
        }else if arg.starts_with("--") {
            bail!("Unknown arg: {arg}");
        }else if arg.starts_with("-") {
            bail!("Unknown option: {arg}");
        }else if file.is_none() {
            file = Some(arg);
        }else {
            bail!("Unexpected value: {arg}");
        }
    }

    let Some(file) = file else {
        bail!("No file specified");
    };

    // Read the contents
    let content = std::fs::read_to_string(&file)?;

    // Parse
    let ast = hulkompiler::ast::Parser::parse(&content)?;

    // Transform
    let tr = hulkompiler::hir::TypeChecker::transform(&ast)?;

    // Emit C
    println!("{tr:?}");
    todo!();

    Ok(())
}

fn main() -> Result<()> {
    // Parse the args
    let mut args = args();

    // Get the binary name
    let Some(bin) = args.next() else {
        bail!("Bad args (expected binary name, found nothing)");
    };
    let binname = Path::new(&bin)
        .file_name()
        .map(OsStr::to_string_lossy)
        .unwrap_or("hulkompiler".into());

    // Parse the args
    while let Some(arg) = args.next() {
        if arg == "-h" || arg == "--help" {
            // Show the help menu
            cmd_help(&binname);
            return Ok(());
        } else if arg == "dump-lex" {
            return cmd_dumplex(&binname, args);
        } else if arg == "dump-ast" {
            return cmd_dumpast(&binname, args);
        } else if arg == "emit-c" {
            return cmd_emitc(&binname, args);
        } else if arg.starts_with("--") {
            bail!("Unknown arg {arg}, try --help");
        } else if arg.starts_with("-") {
            bail!("Unknown option {arg}, try --help");
        } else {
            bail!("Unexpected command {arg}, try --help");
        }
    }

    bail!("No command specified, try --help")
}
