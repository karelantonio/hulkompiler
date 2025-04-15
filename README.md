# HULK Compiler

Toy compiler of the HULK programming language(s) specified [here](https://github.com/matcom/hulk).

Note that this project is not meant to any serious use, just focused on learning how to write compilers.

## Table of contents

1. [Install on linux](#install-on-linux)


## Install on linux

First install cargo via your system's package manager (`apt` on most debian-based systems, `pacman` on arch et cetera) or via `rustup`.

Check the [Cargo Book](https://doc.rust-lang.org/cargo/getting-started/installation.html) to get more details on how to install.

Then clone this repository and inside, run the following command:

```bash
cargo build --release
```

This will download the required dependencies and compile the project.

To use the binary you can copy `target/release/hulkompiler` anywhere you want (preferably in your path).

Save this simple program to test it out in a file called `whatever.hk`:

```
function say_hello(name: String): Number {
    print("I wont say hello");

    1;
}

print(1 + say_hello(" :D  "));
```

And run it with:

```bash
./target/release/hulkcompiler emit-py whatever.hk | python3 -
```
