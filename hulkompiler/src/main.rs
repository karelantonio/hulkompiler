use anyhow::{bail, Result};
use std::{
    env::{args, Args},
    ffi::OsStr,
    path::Path,
};

/// An argument
pub struct Arg {
    id: usize,
    short: Option<&'static str>,
    long: &'static str,
    help: Option<&'static str>,
}

/// A value
pub struct Value {
    id: usize,
    name: &'static str,
    help: Option<&'static str>,
}

/// Could not find argument/option/value
#[derive(Debug, thiserror::Error)]
pub enum ArgError {
    #[error("No args provided")]
    NoArgs,
    #[error("Unexpected option: {0}, try --help")]
    UnexpectedOption(String),
    #[error("Unexpected argument: {0}, try --help")]
    UnexpectedArg(String),
    #[error("Unexpected value: {0}, try --help")]
    UnexpectedValue(String),
    #[error("Missing value: {0}")]
    MissingValue(String),
}

/// Simple argument parser
pub struct ArgParser {
    binname: String,
    cmd_prefix: String,
    desc: Option<String>,
    args: Vec<Arg>,
    values: Vec<Value>,
    procargs: Option<Args>,
    found_values: usize,
}

impl ArgParser {
    fn done(self) -> Result<Args, ArgError> {
        self.procargs.ok_or(ArgError::NoArgs)
    }

    fn new(name: &str, prefix: Option<&str>, desc: Option<&str>) -> Self {
        Self {
            binname: name.into(),
            cmd_prefix: match prefix {
                Some(v) => format!(" {v}"),
                _ => "".into(),
            },
            desc: desc.map(|e| e.to_string()),
            args: vec![Arg {
                id: 0,
                short: Some("-h"),
                long: "--help",
                help: Some("Show this help message"),
            }],
            values: Vec::new(),
            procargs: None,
            found_values: 0,
        }
    }

    fn feed(&mut self, args: Args) {
        self.procargs = Some(args);
    }

    fn push_arg(
        &mut self,
        short: Option<&'static str>,
        long: &'static str,
        help: Option<&'static str>,
    ) -> usize {
        let id = self.args.len() + self.values.len();
        self.args.push(Arg {
            id,
            short,
            long,
            help,
        });
        id
    }

    fn push_value(&mut self, name: &'static str, help: Option<&'static str>) -> usize {
        let id = self.args.len() + self.values.len();
        self.values.push(Value { id, name, help });
        id
    }

    /// Show the help message and exit
    fn show_help(&self) {
        // Print the usage
        // The values
        let vals = self.values.iter().map(|e| e.name).collect::<Vec<_>>();
        println!(
            " Usage: {}{} [OPTIONS] {}",
            self.binname,
            self.cmd_prefix,
            &vals.join(" ")
        );
        println!();

        if let Some(desc) = &self.desc {
            println!("{desc}");
            println!();
        }

        println!(" Options:");
        for arg in self.args.iter() {
            if let Some(shrt) = arg.short {
                println!("  {shrt}, {}", arg.long);
            } else {
                println!("  {}", arg.long);
            }

            if let Some(help) = arg.help {
                println!("    {help}");
            } else {
                println!();
            }
        }

        if self.values.len() > 0 {
            println!(" Values:");
            for val in self.values.iter() {
                println!("  {}", val.name);

                if let Some(help) = val.help {
                    println!("    {help}");
                } else {
                    println!();
                }
            }
        }

        std::process::exit(0);
    }

    fn next(&mut self) -> Result<Option<(usize, String)>, ArgError> {
        let Some(ref mut args) = self.procargs else {
            return Err(ArgError::NoArgs);
        };

        let Some(arg) = args.next() else {
            // No more args
            return if self.found_values < self.values.len() {
                Err(ArgError::MissingValue(self.values[0].name.into()))
            } else {
                Ok(None)
            };
        };

        for uarg in self.args.iter() {
            if Some(arg.as_str()) == uarg.short || arg == uarg.long {
                if uarg.id == 0 {
                    // Is help
                    self.show_help();
                }

                // Matched !
                return Ok(Some((uarg.id, arg)));
            }
        }

        // Check if is an arg
        if arg.starts_with("--") {
            return Err(ArgError::UnexpectedArg(arg));
        } else if arg.starts_with("-") {
            return Err(ArgError::UnexpectedOption(arg));
        }

        // Is a value
        if self.found_values >= self.values.len() {
            return Err(ArgError::UnexpectedValue(arg));
        }

        // Is a valid value
        let id = self.values[self.found_values].id;
        self.found_values += 1;

        Ok(Some((id, arg)))
    }
}

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
    let res = hulkompiler::ast::Parser::parse(&content)?;

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
    let ast = hulkompiler::ast::Parser::parse(&content)?;

    // Transform
    let tr = hulkompiler::hir::TypeChecker::transform(&ast)?;

    // Emit Py
    println!("{}", hulkompiler::emit::py::Emitter::emit(&tr));

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
        Some("Must be one of: emit-py, dump-ast, or dump-lex"),
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
        } else {
            bail!("Unknown command: {val}");
        }
    }

    // Unreachable
    bail!("No command specified")
}
