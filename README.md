# HULK Compiler

Toy compiler of the HULK programming language(s) specified [here](https://github.com/matcom/hulk).

Note that this project is not meant to any serious use, just focused on learning how to write compilers.

See the [examples](examples) directory to get a glimpse of the HULK language.

## Table of contents

1. [Install on linux](#install-on-linux)
2. [Structure of the compiler](#structure-of-the-compiler)
3. [Tokenizer/Lexer](#tokenizerlexer)
4. [Parsing](#parsing)
5. [HIR](#hir)
6. [Code Generation](#code-generation)
7. [License](#license)


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

## Structure of the compiler

The compiler is structured in several compilation staged like blocks, each of these blocks accepts the result from the previous stage, like the following diagram:

```
      Source
         |
         v
 +-----------------+
 | Tokenizer/Lexer |
 +-----------------+
         |
   (Stream of tokens)
         |
         v
 +-----------------+
 | Parsing         |
 +-----------------+
         |
 (Code as a data structure)
         |
         v
 +----------------+
 | HIR            |
 +----------------+
         |
     (another data structure
     with with more meaningful
   information on each of its nodes)
         |
         v
 +----------------+
 | Code generation|
 +----------------+

```

## Tokenizer/Lexer

This stage transforms the source code (given as a stream of bytes) into more meaningful objects called "tokens". Each of these tokens represents a little abstraction over the data. By itself, its not very useful thats why we need to interpret them and transform into some higher lever data structure which we wan work with. An example of these tokens are:

```
Data:
   print("hello world");


The result tokens:
  Id    (Identifier: "print", the name of the function)
  LPar  (Left parenthesis)
  Str   (The string "hello world")
  RPar  (Right parenthesis)
  Semicolon ( ; )
```

All the tokens are located in the `lex.rs` file.

## Parsing

This stage, like the previous transorms a stream (of tokens) into something more meaningful, an abstract syntax tree. We represent the code as a tree, with each leave representing some syntax element. Here we perform a "Syntax check", where we verify if your program is sintactically correct. To get a better understanding, see the following example:

```
Data:
   print("hello world");

Tokens:
   Id, LPar, Str, RPar, Semicolon

Then it gets transformed into a data structure like the following:

  FunctionCall {
     fun_name: "print",
     args: [ Const("hello world") ]
  }
```

The parsing process and data structures are located inside the `ast.rs` file.

## HIR

The HIR (Higher-level intermediate representation) is the result of a transformation to the previous type, where we map each node of the previous AST (abstract syntax tree) into types with even more useful information (like result types, references to functions et cetera). In this stage we perform "Type checking", a verification that your program is semantically correct. See the following example:

```
Data:
   sin(1)^2

Tokens:
   Id, LPar, Id, RPar, Pow, Num

AST:
   BinaryOperation {
      op: Operation::Pow,
      left: FunctionCall {
         fun_name: "sin",
         args: [ Num(1) ]
      },
      right: Num(2)
   }

After type checking, the HIR:
   BinaryOperation {
      op: Operation::Pow,
      ty: Type::Number,
      left: FunctionCall {
         fun_ref: FunctionId(...),
         ty: Type::Number,
         args: [
            Constant {
               const_ref: ConstantId(...),
               ty: Type::Number
            }
         ]
      },
      right: Constant {
         const_ref: ConstantId(...),
         ty: Type::Number
      }
   }
```

Seems like we make the problem worse but this is actually a very useful step, it make A LOT easier the following stage

## Code Generation

In this step we take the previour HIR and transform into some intermediate language. In our case we trasnform into Python code, which can be run like in the first example. Its very language-dependent so there is no general way to explain the logic. To get a better understanding see the `emit` module.

## License

This project is licensed under GNU General Public License v3:

```
    hulkompiler (toy project)
    Copyright (C) 2025 Karel Gonzalez Zaldivar

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
```

For more information see [LICENSE.txt](LICENSE.txt)
