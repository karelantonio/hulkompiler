
use std::env::Args;

/// An argument
pub struct Arg {
    pub id: usize,
    pub short: Option<&'static str>,
    pub long: &'static str,
    pub help: Option<&'static str>,
}

/// A value
pub struct Value {
    pub id: usize,
    pub name: &'static str,
    pub help: Option<&'static str>,
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
    pub fn done(self) -> Result<Args, ArgError> {
        self.procargs.ok_or(ArgError::NoArgs)
    }

    pub fn new(name: &str, prefix: Option<&str>, desc: Option<&str>) -> Self {
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

    pub fn feed(&mut self, args: Args) {
        self.procargs = Some(args);
    }

    pub fn push_arg(
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

    pub fn push_value(&mut self, name: &'static str, help: Option<&'static str>) -> usize {
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

    pub fn next(&mut self) -> Result<Option<(usize, String)>, ArgError> {
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
        } else if arg != "-" && arg.starts_with("-") {
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
