# bmatcher - Your Easy Binary Pattern Matcher &emsp; [![Latest Version]][crates.io] [![License: GPL v3]](./LICENSE) [![GitHub build status]][actions]

[license: gpl v3]: https://img.shields.io/badge/License-GPLv3-blue.svg
[latest version]: https://img.shields.io/crates/v/bmatcher.svg
[crates.io]: https://crates.io/crates/bmatcher
[github build status]: https://github.com/WolverinDEV/bmatcher/actions/workflows/rust.yml/badge.svg?branch=master
[actions]: https://github.com/WolverinDEV/bmatcher/actions/workflows/rust.yml

`bmatcher` is a flexible and efficient binary pattern matching library designed to help you search and match binary data. 
It is `no_std` compatible by default, making it suitable for embedded, kernel, or hypervisor environments where standard library usage is restricted.

## Motivation

Reverse engineering is challenging.
When you identify an interesting address, such as a function or global variable, you don't want to lose all that effort when the program is updated.

The good news is that, during updates, programs usually don't change drastically. While some functions and data may be altered, much of the program remains unchanged.
However, this means that the unchanged parts might be moved to different addresses.

This is where patterns come in.
Patterns allow you to track these interesting parts of a program, even as it evolves and updates.
By using patterns, you can identify specific functions, data references, or other critical locations, regardless of where they end up after a program update.

## Getting Started

To use `bmatcher`, add it as a dependency in your `Cargo.toml`:

```toml
[dependencies]
bmatcher = "0.3"
```

## Creating a pattern

An exhausive overview of the pattern syntax and operads can be found [here](./bmatcher/GRAMMA.MD).

## Basic Usage

Here's a simple example demonstrating how to use bmatcher to match a call signature binary pattern:

```rust
let data: &[u8] = ...;
let pattern = pattern!("
    /*
     * call my_function
     * $ = follow 4 byte relative jump
     * ' = save cursor position to the matched stack
     */
    E8 $ { ' }

    /* mov QWORD PTR [rsp], rax */
    48 89 04 24

    /* jmp somewhere */
    E9 [4]
");

let mut matcher = bmatcher::execute(&data, &pattern);
let Some(match_stack) = matcher.next_match() else {
    panic!("failed to find pattern");
};

println!("Matched at location 0x{:X}", match_stack[0]);
println!("Target function located at 0x{:X}", match_stack[1]);
```
