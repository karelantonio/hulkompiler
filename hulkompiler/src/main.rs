
/// Parse args
mod args;

use args::ArgParser;
use anyhow::{bail, Result};
use std::{
    env::{args, Args},
    ffi::OsStr,
    path::Path,
};
use hulkompiler_emit_py as emit_py;
use hulkompiler_ast as ast;
use hulkompiler_hir as hir;
use hulkompiler_emit_cpp as emit_cpp;


/// Dump the lex result
fn cmd_dumplex(binname: &str, args: Args) -> Result<()> {
    let mut file = None;
    let mut wide = false;

    let mut parser = ArgParser::new(binname, Some("dump-lex"), None);
    parser.feed(args);
    let o_wide = parser.push_arg(Some("-w"), "--wide", Some("Print the result wider"));
    let o_file = parser.push_value("file", Some("The source file"));

    while let Some((arg, val)) = parser.next()? {
        if arg == o_wide {
            wide = true;
        } else if arg == o_file {
            file = Some(val);
        }
    }

    let file = file.expect("Should not be None, see ArgParser");

    // Read the contents
    let data = std::fs::read_to_string(file)?;

    // Lex the contents
    let res = ast::lex::tokenize_data(&data)?;
    println!("Result:");
    if wide {
        println!("{res:#?}");
    } else {
        println!("{res:?}");
    }

    Ok(())
}

/// Dump to stdout the abstract syntax tree
fn cmd_dumpast(binname: &str, args: Args) -> Result<()> {
    let mut file = None;
    let mut wide = false;

    let mut parser = ArgParser::new(binname, Some("dump-ast"), None);
    parser.feed(args);
    let o_wide = parser.push_arg(Some("-w"), "--wide", Some("Print the result wider"));
    let o_file = parser.push_value("file", Some("The source file"));

    while let Some((arg, val)) = parser.next()? {
        if arg == o_wide {
            wide = true;
        } else if arg == o_file {
            file = Some(val);
        }
    }

    let file = file.expect("Should not be None, see ArgParser");

    // Read the file
    let content = std::fs::read_to_string(file)?;

    // Parse
    let res = ast::Parser::parse(&content)?;

    println!("Result:");
    if wide {
        println!("{res:#?}");
    } else {
        println!("{res:?}");
    }

    Ok(())
}

fn cmd_emitpy(binname: &str, args: Args) -> Result<()> {
    let mut file = None;

    let mut parser = ArgParser::new(binname, Some("emit-py"), None);
    let _p_file = parser.push_value("file", Some("The source file"));
    parser.feed(args);

    while let Some((_arg, val)) = parser.next()? {
        file = Some(val);
    }

    let file = file.expect("Should not be None, see ArgParser"); // Its ok

    // Read the contents
    let content = std::fs::read_to_string(&file)?;

    // Parse
    let ast = ast::Parser::parse(&content)?;

    // Transform
    let tr = hir::TypeChecker::transform(&ast)?;

    // Emit Py
    println!("{}", emit_py::Emitter::emit(&tr));

    Ok(())
}

fn cmd_emitcpp(binname: &str, args: Args) -> Result<()> {
    let mut file = None;

    let mut parser = ArgParser::new(binname, Some("emit-cpp"), None);
    let _p_file = parser.push_value("file", Some("The source file"));
    parser.feed(args);

    while let Some((_arg, val)) = parser.next()? {
        file = Some(val);
    }

    let file = file.expect("Should not be None, see ArgParser"); // Its ok

    // Read the contents
    let content = std::fs::read_to_string(&file)?;

    // Parse
    let ast = ast::Parser::parse(&content)?;

    // Transform
    let tr = hir::TypeChecker::transform(&ast)?;

    // Emit Py
    println!("{}", emit_cpp::Emitter::emit(&tr));

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

    let mut parser = ArgParser::new(&binname, None, None);
    let _opt_cmd = parser.push_value(
        "command",
        Some("Must be one of: emit-cpp, emit-py, dump-ast, or dump-lex"),
    );
    parser.feed(args);

    // Parse the args
    while let Some((_arg, val)) = parser.next()? {
        if val == "dump-lex" {
            return cmd_dumplex(&binname, parser.done()?);
        } else if val == "dump-ast" {
            return cmd_dumpast(&binname, parser.done()?);
        } else if val == "emit-py" {
            return cmd_emitpy(&binname, parser.done()?);
        } else if val == "emit-cpp" {
            return cmd_emitcpp(&binname, parser.done()?);
        } else {
            bail!("Unknown command: {val}");
        }
    }

    // Unreachable
    bail!("No command specified")
}
