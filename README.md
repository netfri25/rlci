# [LOLCODE](http://lolcode.org) interpreter
rewrite of the [original LOLCODE interpreter](https://github.com/justinmeza/lci) in Rust™

### Why rewrite?
I wanted the interpreter to support multithreading. I tried implementing it in the original C interpreter but it had too many segfaults (my skill issue) and I was too lazy to figure out how to fix them and how to make the interpreter support threads safely. (it also had some memory leaks)

## Requirements
 - [Rust](https://rustup.rs/)

## Installation
```shell
cargo install --path .
```

## Usage
```shell
rlci run file.lol
```

## Specification
the original specification can be found [here](https://github.com/justinmeza/lolcode-spec/blob/master/v1.3/lolcode-spec-v1.3.md), most of it is implemented in this version of the interpreter.\
for a more detailed specification on this version of the interpreter, check out [here](./spec.md).
